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
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module Storage.Notmuch (
    -- * Synopsis
    -- $synopsis

    -- * API
    -- ** Threads
    getThreads
  , getThreadMessages
  , countThreads

    -- ** Messages
  , messageTagModify
  , mailFilepath
  , indexFilePath
  , unindexFilePath

    -- ** Tagging (Labels)
  , ManageTags(..)
  , hasTag
  , tagItem
  , addTags
  , removeTags

    -- ** Database
  , getDatabasePath
  , withDatabase
  ) where

import Control.Monad ((<=<), (>=>), when)
import Data.Function (on)
import Data.Foldable (toList)
import Data.Functor.Compose (Compose(..))

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (MonadError, throwError, ExceptT, runExceptT)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.List (union, nub, sort)
import Data.Maybe (fromMaybe)
import Data.Functor (($>))
import qualified Data.Vector as Vec
import System.Exit (ExitCode(..))
import qualified System.Directory as Directory (removeFile)
import qualified Data.Text as T
import Control.Lens (view, over, set, firstOf, folded, Lens')

import qualified Notmuch

import Types
import Purebred.System.Process (readProcess, proc)
import Purebred.System (tryIO)
import Purebred.Types.Error
import Purebred.Types.IFC (sanitiseText, untaint)

#if defined LAZYVECTOR
import System.IO.Unsafe (unsafeInterleaveIO)
import Purebred.LazyVector
#endif

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

-- | Tag either a 'NotmuchMail' or a 'NotmuchThread'
--
tagItem :: ManageTags a => [TagOp] -> a -> a
tagItem ops mail = foldl (flip applyTagOp) mail ops

haveTagsChanged :: NotmuchMail -> NotmuchMail -> Bool
haveTagsChanged = (/=) `on` (sort . nub . view mailTags)

applyTagOp :: (ManageTags a) => TagOp -> a -> a
applyTagOp (AddTag t) = addTags [t]
applyTagOp (RemoveTag t) = removeTags [t]
applyTagOp ResetTags = setTags []

class ManageTags a  where
    tags :: Lens' a [Tag]

setTags :: (ManageTags a) => [Tag] -> a -> a
setTags = set tags

addTags :: (ManageTags a) => [Tag] -> a -> a
addTags tgs = over tags (`union` tgs)

removeTags :: (ManageTags a) => [Tag] -> a -> a
removeTags tgs = over tags (filter (`notElem` tgs))

hasTag :: (ManageTags a) => Tag -> a -> Bool
hasTag t x = t `elem` view tags x

instance ManageTags NotmuchMail where
  tags = mailTags

instance ManageTags NotmuchThread where
  tags = thTags

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
-- get database.path' in a separate process
--
getDatabasePath :: IO FilePath
getDatabasePath = do
  let cmd = "notmuch"
  let args = ["config", "get", "database.path"]
  (exitc, stdout, err) <- readProcess $ proc cmd args
  case exitc of
    ExitFailure _ -> error (untaint decode err)
    ExitSuccess -> pure (filter (/= '\n') (untaint decode stdout))
  where
      decode = T.unpack . sanitiseText . decodeLenient . LB.toStrict

-- | Return the number of threads for the given query
countThreads ::
     (MonadError Error m, MonadIO m) => T.Text -> FilePath -> m Int
countThreads query fp =
  withDatabaseReadOnly fp $
  flip Notmuch.query (Notmuch.Bare $ T.unpack query)
  >=> Notmuch.queryCountMessages


#if defined LAZYVECTOR
-- | creates a vector of threads from a notmuch search
--
getThreads
  :: (MonadError Error m, MonadIO m)
  => T.Text
  -> NotmuchSettings FilePath
  -> m (V (Toggleable NotmuchThread))
getThreads s settings =
  withDatabaseReadOnly (view nmDatabase settings) $
    flip Notmuch.query (Notmuch.Bare $ T.unpack s)
    >=> Notmuch.threads
    >=> liftIO . fmap (fromList 128) . lazyTraverse (fmap (False,) . threadToThread)

lazyTraverse :: (a -> IO b) -> [a] -> IO [b]
lazyTraverse f =
  foldr (\x ys -> (:) <$> f x <*> unsafeInterleaveIO ys) (pure [])

#else
getThreads
  :: (MonadError Error m, MonadIO m)
  => T.Text
  -> NotmuchSettings FilePath
  -> m (Vec.Vector (Toggleable NotmuchThread))
getThreads s settings =
  withDatabaseReadOnly (view nmDatabase settings) $
    flip Notmuch.query (Notmuch.Bare $ T.unpack s)
    >=> Notmuch.threads
    >=> liftIO . fmap Vec.fromList . traverse (fmap (False,) . threadToThread)
#endif

-- | Returns a vector of *all* messages belonging to the list of threads
--
getThreadMessages
  :: (MonadError Error m, MonadIO m, Traversable t)
  => FilePath
  -> t NotmuchThread
  -> m (Vec.Vector (Toggleable NotmuchMail))
getThreadMessages fp ts = withDatabaseReadOnly fp go
  where
    go db = do
      msgs <-
        Data.Functor.Compose.Compose  -- 'collapse' nested 't ([] a)'
        <$> traverse (Notmuch.messages <=< getThread db . view thId) ts
      mails <- traverse (liftIO . messageToMail) msgs
      pure . Vec.fromList . toList $ fmap (False,) mails


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
