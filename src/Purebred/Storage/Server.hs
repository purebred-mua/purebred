-- This file is part of purebred
-- Copyright (C) 2022 Fraser Tweedale and Róman Joost
--
-- purebred is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Purebred.Storage.Server
  (
    Server
  , startServer
  , enqueue
  , Command(Command)
  , ResultBox
  ) where

import Control.Concurrent (ThreadId, forkOS)
import Control.Concurrent.STM
  ( atomically, orElse, retry
  , TMVar, newEmptyTMVar, readTMVar, tryPutTMVar
  , newTQueue, readTQueue, writeTQueue
  )
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Foldable (traverse_)

import Control.Concurrent.STM.Delay

import qualified Notmuch

import Purebred.Types.Error (Error)

-- Right now we don't use the ThreadId for anything.
-- But later it may be useful e.g. to detect if the server
-- thread has crashed and restart it.
data Server = Server ThreadId (Command -> IO ())

-- | Enqueue a command
enqueue :: Server -> Command -> IO ()
enqueue (Server _tid sink) = sink

type ResultBox a = TMVar (Either Error a)

data Command
  -- | Tell the server to execute an action that takes the database
  -- as an argument.  The server shall (try to) put the result in
  -- the 'ResultBox', if given.  If the put fails because the
  -- 'ResultBox' is non-empty, the result is silently discarded.
  = forall a. Command (Notmuch.Database Notmuch.RW -> ExceptT Error IO a) (Maybe (ResultBox a))
  | Open (ResultBox ())
  | Close

-- | Default timeout duration (2 seconds)
defaultTimeout :: Int
defaultTimeout = 2 * 1000000

-- | Start database server, given path to database
--
-- Start the server and immediately open the database. Return
-- a @Right Server@ upon success or @Left Error@ upon failure.
--
startServer :: FilePath -> IO (Either Error Server)
startServer dbpath = do
  -- create required STM structures
  (q, ok) <- atomically $ do
    q <- newTQueue
    ok <- newEmptyTMVar
    writeTQueue q (Open ok) -- enqueue an initial Open command
    pure (q, ok)

  -- define the server loop
  let
    loop :: Maybe (Notmuch.Database Notmuch.RW, Delay) -> IO ()
    loop mDb = do
      command <-
        atomically $
          readTQueue q
          `orElse` maybe retry (fmap (const Close) . waitDelay . snd) mDb

      case (command, mDb) of
        (Close, _) ->
          -- Explicit close request.
          -- Either there is no handle (nothing to do), or there is,
          -- and we need to forget about it so it will be GC'd (and
          -- closed by the finalizer).  We /could/ 'cancelDelay',
          -- but the 'Close' command almost certainly arose from the
          -- existing timeout, so don't bother.
          loop Nothing

        (Open rbox, Just _) -> do
          -- Explicit open request, but we already have a db handle.
          -- Nothing to do but write the success signal to the ResultBox.
          -- This is a "shouldn't happen" case so don't update the timer.
          _ <- atomically $ tryPutTMVar rbox (Right ())
          loop mDb

        (Open rbox, Nothing) ->
          -- Explicit open request, and we do not have a handle.
          -- Open the database and put the handle in the ResultBox.
          -- Start a close timer.
          runExceptT (Notmuch.databaseOpen dbpath) >>= \case
            Left err -> do
              _ <- atomically $ tryPutTMVar rbox (Left err)
              loop Nothing
            Right db -> do
              _ <- atomically $ tryPutTMVar rbox (Right ())
              delay <- newDelay defaultTimeout
              loop $ Just (db, delay)

        (Command core mrbox, Nothing) ->
          -- General command, and no active database handle.
          -- Open the database, then run the command.
          runExceptT (Notmuch.databaseOpen dbpath) >>= \case
            Left err -> do
              traverse_ (\rbox -> atomically (tryPutTMVar rbox (Left err))) mrbox
              loop Nothing
            Right db -> do
              r <- runExceptT (core db)
              traverse_ (\rbox -> atomically (tryPutTMVar rbox r)) mrbox
              delay <- newDelay defaultTimeout
              loop $ Just (db, delay)

        (Command core mrbox, Just (db, delay)) -> do
          -- General command, and we already have a db handle.
          -- Run the command and reset the timer.
          r <- runExceptT (core db)
          traverse_ (\rbox -> atomically (tryPutTMVar rbox r)) mrbox
          updateDelay delay defaultTimeout
          loop $ Just (db, delay)

  -- start the server thread
  tid <- forkOS $ loop Nothing

  -- await the 'ok' ResultBox.  If 'Right ()', replace () with
  -- the Server
  let server = Server tid (atomically . writeTQueue q)
  atomically . fmap (server <$) $ readTMVar ok

