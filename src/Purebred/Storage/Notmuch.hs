-- This file is part of purebred
-- Copyright (C) 2017-2020 Róman Joost
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
{-# LANGUAGE FlexibleInstances #-}

module Purebred.Storage.Notmuch (
    -- * Synopsis
    -- $synopsis

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

    -- ** Database
  , getDatabasePath
  , withDatabase
  ) where

import Control.Monad ((<=<), (>=>), when)
import Data.Function (on)
import Data.Foldable (toList)
import Data.Functor.Compose (Compose(..))
import System.IO.Unsafe (unsafeInterleaveIO)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (MonadError, throwError, ExceptT, runExceptT)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.List (nub, sort)
import Data.Maybe (fromMaybe)
import Data.Functor (($>))
import qualified Data.Vector as Vec
import System.Exit (ExitCode(..))
import qualified System.Directory as Directory (removeFile)
import qualified Data.Text as T
import Control.Lens (view, firstOf, folded)

import qualified Notmuch

import Purebred.Types
import Purebred.Storage.Tags
import Purebred.System.Process (readProcess, proc)
import Purebred.System (tryIO)
import Purebred.Types.Error
import Purebred.Types.IFC (sanitiseText, untaint)
import Purebred.Types.Items

{- $synopsis

The purpose of this module is to provide a bridge between the
low-level API of notmuch and the higher level functionality in
Purebred. It propagates errors picked up by callers in order to be set
in the 'AppState'. The module also serves as a translation between
notmuch data types and Purebreds. The latter are used in the UI.
-}


-- | apply tag operations on all given mails and write the resulting tags to the
-- database
messageTagModify
  :: (Traversable t, MonadError Error m, MonadIO m)
  => FilePath  -- ^ database
  -> [TagOp]
  -> t NotmuchMail
  -> m (t NotmuchMail)
messageTagModify dbpath ops xs =
  withDatabase dbpath (\db -> applyTags ops db xs)

applyTags
    :: (MonadError Error m, MonadIO m, Traversable t)
    => [TagOp]
    -> Notmuch.Database Notmuch.RW
    -> t NotmuchMail
    -> m (t NotmuchMail)
applyTags ops db = traverse $ \m -> do
  let m' = tagItem ops m
  when (haveTagsChanged m m')
    (tagsToMessage (view mailTags m') (view mailId m') db)
  pure m'

haveTagsChanged :: NotmuchMail -> NotmuchMail -> Bool
haveTagsChanged = (/=) `on` (sort . nub . view mailTags)

-- | A helper function for opening and performing work on a database.
-- The database is not explicitly closed (GC will take care of that).
--
withDatabase
  :: (Notmuch.AsNotmuchError e, Notmuch.Mode a, MonadError e m, MonadIO m)
  => FilePath
  -> (Notmuch.Database a -> ExceptT e IO c)
  -> m c
withDatabase dbpath f =
  Notmuch.databaseOpen dbpath >>= liftIO . runExceptT . f
  >>= either throwError pure


-- | Same as 'withDatabase', but the database connection
-- is read-only
withDatabaseReadOnly
  :: (Notmuch.AsNotmuchError e, MonadError e m, MonadIO m)
  => FilePath
  -> (Notmuch.Database Notmuch.RO -> ExceptT e IO c)
  -> m c
withDatabaseReadOnly = withDatabase

-- | Returns the absolute path to the email. Typically used by the
-- email parser.
--
mailFilepath
  :: (MonadError Error m, MonadIO m)
  => NotmuchMail -> FilePath -> m FilePath
mailFilepath m dbpath =
  withDatabaseReadOnly dbpath go
  where
    go db = getMessage db (view mailId m) >>= Notmuch.messageFilename

tagsToMessage
  :: (MonadError Error m, MonadIO m)
  => [Notmuch.Tag] -> B.ByteString -> Notmuch.Database Notmuch.RW -> m ()
tagsToMessage xs id' db = getMessage db id' >>= Notmuch.messageSetTags xs

-- | Get message by message ID, throwing MessageNotFound if not found
--
getMessage
  :: (MonadError Error m, MonadIO m)
  => Notmuch.Database mode -> B.ByteString -> m (Notmuch.Message 0 mode)
getMessage db msgId =
  Notmuch.findMessage db msgId
  >>= maybe (throwError (MessageNotFound msgId)) pure

messageToMail
    :: Notmuch.Message n a
    -> IO NotmuchMail
messageToMail m = do
    tgs <- Notmuch.tags m
    NotmuchMail
      <$> (fixupWhitespace . decodeLenient . fromMaybe "" <$> Notmuch.messageHeader "Subject" m)
      <*> (decodeLenient . fromMaybe "" <$> Notmuch.messageHeader "From" m)
      <*> Notmuch.messageDate m
      <*> pure tgs
      <*> Notmuch.messageId m

-- | Returns the notmuch database path by executing 'notmuch config
-- get database.path' in a separate process.  If the process terminates
-- abnormally, returns an empty string.
--
getDatabasePath :: IO FilePath
getDatabasePath = do
  let cmd = "notmuch"
  let args = ["config", "get", "database.path"]
  (exitc, stdout, _err) <- readProcess $ proc cmd args
  pure $ case exitc of
    ExitFailure _ -> ""
    ExitSuccess -> filter (/= '\n') (untaint decode stdout)
  where
      decode = T.unpack . sanitiseText . decodeLenient . LB.toStrict

-- | Return the number of messages for the given query
countMessages ::
     (MonadError Error m, MonadIO m) => T.Text -> FilePath -> m Int
countMessages query fp =
  withDatabaseReadOnly fp $
  flip Notmuch.query (Notmuch.Bare $ T.unpack query)
  >=> Notmuch.queryCountMessages


-- | creates a vector of threads from a notmuch search
--
getThreads
  :: (MonadError Error m, MonadIO m)
  => T.Text
  -> NotmuchSettings
  -> m (Items NotmuchThread)
getThreads s settings =
  withDatabaseReadOnly (view nmDatabase settings) $
    flip Notmuch.query (Notmuch.Bare $ T.unpack s)
    >=> Notmuch.threads
    -- note: we use lazyTraverse and "chunked" construction, but
    -- whether it will actually be lazy depends on the variant of
    -- 'Items' in use.  For a Vector, it is strict and non-chunked.
    >=> liftIO . fmap (fromList 128) . lazyTraverse threadToThread

lazyTraverse :: (a -> IO b) -> [a] -> IO [b]
lazyTraverse f =
  foldr (\x ys -> (:) <$> f x <*> unsafeInterleaveIO ys) (pure [])

-- | Returns a vector of *all* messages belonging to the list of threads
--
getThreadMessages
  :: (MonadError Error m, MonadIO m, Traversable t)
  => FilePath
  -> t NotmuchThread
  -> m (Vec.Vector NotmuchMail)
getThreadMessages fp ts = withDatabaseReadOnly fp go
  where
    go db = do
      msgs <-
        Data.Functor.Compose.Compose  -- 'collapse' nested 't ([] a)'
        <$> traverse (Notmuch.messages <=< getThread db . view thId) ts
      mails <- traverse (liftIO . messageToMail) msgs
      pure . Vec.fromList . toList $ mails


-- | retrieve a given thread from the notmuch database by it's id
-- Note: The notmuch API does not provide a designated endpoint for retrieving
-- the thread by it's ID. We're cheating here by simply querying for the given
-- thread id.
--
getThread
  :: (MonadError Error m, MonadIO m)
  => Notmuch.Database mode -> B.ByteString -> m (Notmuch.Thread mode)
getThread db tid = do
  t <- Notmuch.query db (Notmuch.Thread tid) >>= Notmuch.threads
  maybe (throwError (ThreadNotFound tid)) pure (firstOf folded t)

threadToThread
  :: Notmuch.Thread a
  -> IO NotmuchThread
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

fixupWhitespace :: T.Text -> T.Text
fixupWhitespace = T.map f . T.filter (/= '\n')
  where f '\t' = ' '
        f c = c

indexFilePath ::
     (MonadError Error m, MonadIO m)
  => FilePath -- ^ database
  -> FilePath -- ^ mail
  -> [Tag]
  -> m ()
indexFilePath dbpath fp tgs =
  withDatabase
    dbpath
    (\db -> Notmuch.indexFile db fp >>= Notmuch.messageSetTags tgs)

unindexFilePath ::
     (MonadError Error m, MonadIO m)
  => FilePath -- ^ database
  -> FilePath -- ^ mail
  -> m ()
unindexFilePath dbpath fp =
  withDatabase
    dbpath
    (\db -> Notmuch.removeFile db fp *> tryIO (Directory.removeFile fp $> ()))
