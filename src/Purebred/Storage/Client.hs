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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Purebred.Storage.Client
  (
    -- * API
    -- ** Threads
    getThreads
  , getThreadMessages
  , countMessages

    -- ** Messages
  , messageTagModify
  , mailFilepath
  , indexFilePath
  , unindexFilePath

  -- * Re-exports
  , Server
  ) where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (atomically , newEmptyTMVarIO, readTMVar)
import Control.Lens (firstOf, folded, view)
import Control.Monad ((<=<), void, when)
import Control.Monad.Except (ExceptT, MonadError, liftEither, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (toList)
import Data.Function (on)
import Data.Functor.Compose (Compose(..))
import Data.Maybe (fromMaybe)
import Data.List (nub, sort)
import qualified Data.Text as T
import Data.Traversable (for)
import qualified System.Directory
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Data.Vector as V
import qualified Notmuch

import Purebred.Storage.Server
import Purebred.Storage.Tags (TagOp, tagItem, tags)
import Purebred.System (tryIO)
import Purebred.Types (NotmuchThread(..), NotmuchMail(..), decodeLenient, mailId, thId)
import Purebred.Types.Error (Error(MessageNotFound, ThreadNotFound))
import Purebred.Types.Items (Items, fromList)

---
--- Interacting with server
---

type Call params result =
  forall m. (MonadIO m, MonadError Error m)
  => params -> Server -> m result

type Call2 param1 param2 result =
  forall m. (MonadIO m, MonadError Error m)
  => param1 -> param2 -> Server -> m result

-- | Send command and await result
runCommand
  :: (MonadError Error m, MonadIO m)
  => (Notmuch.Database Notmuch.RW -> ExceptT Error IO a) -> Server -> m a
runCommand core server = do
  result <- liftIO $ do
    box <- newEmptyTMVarIO
    enqueue server $ Command core (Just box)
    atomically $ readTMVar box
  liftEither result

-- | Enqueue command and return immediately, discarding result
_sendCommand
  :: (MonadIO m)
  => (Notmuch.Database Notmuch.RW -> ExceptT Error IO a) -> Server -> m ()
_sendCommand core server = void . liftIO . enqueue server $ Command core Nothing

-- | Enqueue command and fork an unbound thread to handle the result.
-- Returns 'ThreadId' of the result handler thread.
_runCommandWithHandler
  :: (MonadIO m)
  => (Notmuch.Database Notmuch.RW -> ExceptT Error IO a)
  -> (Either Error a -> IO ())
  -> Server -> m ThreadId
_runCommandWithHandler core handler server =
  liftIO $ do
    box <- newEmptyTMVarIO
    enqueue server $ Command core (Just box)
    forkIO $ do
      r <- atomically $ readTMVar box
      handler r

---
--- Database actions
---

-- | Apply tag operations on all given mails and write the resulting
-- tags to the database
messageTagModify
  :: (Traversable t)
  => Call2 [TagOp] (t NotmuchMail) (t NotmuchMail)
messageTagModify ops msgs = runCommand $ \db ->
  for msgs $ \m -> do
    let m' = tagItem ops m
    -- only act if tags actually changed
    when (((/=) `on` (sort . nub . view tags)) m m') $
      getMessage db (view mailId m')
      >>= Notmuch.messageSetTags (view tags m')
    pure m'

-- | Returns the absolute path to the email. Typically used by the
-- email parser.
--
mailFilepath :: Call NotmuchMail FilePath
mailFilepath m = runCommand $ \db ->
  getMessage db (view mailId m) >>= Notmuch.messageFilename

-- | Return the number of messages for the given free-form search term
countMessages :: Call T.Text Int
countMessages expr = runCommand $ \db ->
  Notmuch.query db (Notmuch.Bare $ T.unpack expr)
  >>= Notmuch.queryCountMessages

-- | Creates a vector of threads from a free-form search term
--
getThreads :: Call T.Text (Items NotmuchThread)
getThreads expr = runCommand $ \db ->
  Notmuch.query db (Notmuch.Bare $ T.unpack expr)
    >>= Notmuch.threads
    -- note: we use lazyTraverse and "chunked" construction, but
    -- whether it will actually be lazy depends on the variant of
    -- 'Items' in use.  For a Vector, it is strict and non-chunked.
    >>= liftIO . fmap (fromList 128) . lazyTraverse threadToThread
  where
  threadToThread :: Notmuch.Thread a -> IO NotmuchThread
  threadToThread m = do
    tgs <- Notmuch.tags m
    auth <- Notmuch.threadAuthors m
    NotmuchThread
      <$> (fixupWhitespace . decodeLenient <$> Notmuch.threadSubject m)
      <*> pure (view Notmuch.matchedAuthors auth)
      <*> Notmuch.threadNewestDate m
      <*> pure tgs
      <*> Notmuch.threadTotalMessages m
      <*> Notmuch.threadId m

-- | Returns a vector of *all* messages belonging to a collection of threads
--
getThreadMessages :: (Traversable t) => Call (t NotmuchThread) (V.Vector NotmuchMail)
getThreadMessages threads = runCommand $ \db -> do
  msgs <- Compose  -- 'collapse' nested 't ([] a)'
          <$> traverse (Notmuch.messages <=< getThread db . view thId) threads
  mails <- traverse (liftIO . messageToMail) msgs
  pure . V.fromList . toList $ mails

  where
  -- Retrieve thread by ID.  libnotmuch does not provide a direct
  -- way to query a thread.  Perform a general query and pop the
  -- first result, or fail if the result is empty.
  --
  getThread db tid = do
    t <- Notmuch.query db (Notmuch.Thread tid) >>= Notmuch.threads
    maybe (throwError (ThreadNotFound tid)) pure (firstOf folded t)

  messageToMail m = do
    tgs <- Notmuch.tags m
    NotmuchMail
      <$> (fixupWhitespace . decodeLenient . fromMaybe "" <$> Notmuch.messageHeader "Subject" m)
      <*> (decodeLenient . fromMaybe "" <$> Notmuch.messageHeader "From" m)
      <*> Notmuch.messageDate m
      <*> pure tgs
      <*> Notmuch.messageId m

indexFilePath :: Call2 FilePath [Notmuch.Tag] ()
indexFilePath path tags_ = runCommand $ \db ->
  Notmuch.indexFile db path >>= Notmuch.messageSetTags tags_

unindexFilePath :: Call FilePath ()
unindexFilePath path = runCommand $ \db ->
  void $ Notmuch.removeFile db path *> tryIO (System.Directory.removeFile path)

---
--- Helper functions
---

lazyTraverse :: (a -> IO b) -> [a] -> IO [b]
lazyTraverse f =
  foldr (\x ys -> (:) <$> f x <*> unsafeInterleaveIO ys) (pure [])

fixupWhitespace :: T.Text -> T.Text
fixupWhitespace = T.map f . T.filter (/= '\n')
  where f '\t' = ' '
        f c = c

-- | Get message by message ID, throwing MessageNotFound if not found
--
getMessage
  :: (MonadError Error m, MonadIO m)
  => Notmuch.Database mode -> Notmuch.MessageId -> m (Notmuch.Message 0 mode)
getMessage db msgId =
  Notmuch.findMessage db msgId
  >>= maybe (throwError (MessageNotFound msgId)) pure
