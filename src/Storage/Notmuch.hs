{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | module for integrating notmuch within purebred
module Storage.Notmuch where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (MonadError, throwError)
import qualified Data.ByteString as B
import Data.Traversable (traverse)
import Data.List (union, notElem)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vec
import System.Process (readProcess)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Types
import Control.Lens (view, over, set, firstOf, folded)

import Notmuch
import Notmuch.Search
import Notmuch.Util (bracketT)

import Error


-- | creates a vector of parsed mails from a not much search
-- Note, that at this point in time only free form searches are supported. Also,
-- we filter out the tag which we use to mark mails as new mails
getMessages
  :: (MonadError Error m, MonadIO m)
  => T.Text
  -> NotmuchSettings FilePath
  -> m (Vec.Vector NotmuchMail)
getMessages s settings =
  bracketT (databaseOpenReadOnly (view nmDatabase settings)) databaseDestroy go
  where go db = do
              msgs <- query db (FreeForm $ T.unpack s) >>= messages
              mails <- liftIO $ mapM messageToMail msgs
              pure $ Vec.fromList mails

mailFilepath
  :: (MonadError Error m, MonadIO m)
  => NotmuchMail -> FilePath -> m FilePath
mailFilepath m dbpath =
  bracketT (databaseOpenReadOnly dbpath) databaseDestroy go
  where
    go db = getMessage db (view mailId m) >>= messageFilename

setNotmuchMailTags
  :: (MonadError Error m, MonadIO m)
  => FilePath
  -> NotmuchMail
  -> m NotmuchMail
setNotmuchMailTags dbpath m = do
  nmtags <- mailTagsToNotmuchTags m
  bracketT (databaseOpen dbpath) databaseDestroy (tagsToMessage nmtags m)
  pure m


tagsToMessage
  :: (MonadError Error m, MonadIO m)
  => [Tag] -> NotmuchMail -> Database RW -> m ()
tagsToMessage xs msg db = getMessage db (view mailId msg) >>= messageSetTags xs

-- | Get message by message ID, throwing MessageNotFound if not found
--
getMessage
  :: (MonadError Error m, MonadIO m)
  => Database mode -> B.ByteString -> m (Message 0 mode)
getMessage db msgId =
  findMessage db msgId
  >>= maybe (throwError (MessageNotFound msgId)) pure

setTags :: NotmuchMail -> [T.Text] -> NotmuchMail
setTags m ts = set mailTags ts m

addTags :: NotmuchMail -> [T.Text] -> NotmuchMail
addTags m ts = over mailTags (`union` ts) m

removeTags :: NotmuchMail -> [T.Text] -> NotmuchMail
removeTags m ts = over mailTags (filter (`notElem` ts)) m

mailTagsToNotmuchTags :: MonadError Error m => NotmuchMail -> m [Tag]
mailTagsToNotmuchTags = traverse (mkTag' . encodeUtf8) . view mailTags
  where mkTag' s = maybe (throwError (InvalidTag s)) pure $ mkTag s

messageToMail
    :: HasTags (Message n a)
    => Message n a
    -> IO NotmuchMail
messageToMail m = do
    tgs <- tags m
    let tgs' = decodeUtf8 . getTag <$> tgs
    NotmuchMail
      <$> (decodeUtf8 . fromMaybe "" <$> messageHeader "Subject" m)
      <*> (decodeUtf8 . fromMaybe "" <$> messageHeader "From" m)
      <*> messageDate m
      <*> pure tgs'
      <*> messageId m

getDatabasePath :: IO FilePath
getDatabasePath = getFromNotmuchConfig "database.path"

getFromNotmuchConfig :: String -> IO String
getFromNotmuchConfig key = do
  let cmd = "notmuch"
  let args = ["config", "get", key]
  stdout <- readProcess cmd args []
  pure $ filter (/= '\n') stdout

-- | creates a vector of threads from a notmuch search
--
getThreads
  :: (HasThreads Query, MonadError Error m, MonadIO m)
  => T.Text
  -> NotmuchSettings FilePath
  -> m (Vec.Vector NotmuchThread)
getThreads s settings =
  bracketT (databaseOpenReadOnly (view nmDatabase settings)) databaseDestroy go
  where
    go db = do
        ts <- query db (FreeForm $ T.unpack s) >>= threads
        t <- liftIO $ traverse threadToThread ts
        pure $ Vec.fromList t

-- | returns a vector of *all* messages belonging to the given thread
--
getThreadMessages
  :: (MonadError Error m, MonadIO m)
  => FilePath
  -> NotmuchThread
  -> m (Vec.Vector NotmuchMail)
getThreadMessages fp t =
  bracketT (databaseOpenReadOnly fp) databaseDestroy go
  where go db = do
          msgs <- getThread db (view thId t) >>= messages
          mails <- liftIO $ traverse messageToMail msgs
          pure $ Vec.fromList mails

-- | retrieve a given thread from the notmuch database by it's id
-- Note: The notmuch API does not provide a designated endpoint for retrieving
-- the thread by it's ID. We're cheating here by simply querying for the given
-- thread id.
--
getThread
  :: (MonadError Error m, MonadIO m)
  => Database mode -> B.ByteString -> m (Thread mode)
getThread db tid = do
  t <- query db (Thread tid) >>= threads
  maybe (throwError (ThreadNotFound tid)) pure (firstOf folded t)

threadToThread
  :: (HasTags (Thread a), HasThread (Thread a))
  => Thread a
  -> IO NotmuchThread
threadToThread m = do
    tgs <- tags m
    auth <- threadAuthors m
    let tgs' = decodeUtf8 . getTag <$> tgs
    NotmuchThread
      <$> (decodeUtf8 <$> threadSubject m)
      <*> (pure $ view matchedAuthors auth)
      <*> threadNewestDate m
      <*> pure tgs'
      <*> threadTotalMessages m
      <*> threadId m
