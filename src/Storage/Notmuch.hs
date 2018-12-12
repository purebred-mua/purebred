{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | module for integrating notmuch within purebred
module Storage.Notmuch where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (MonadError, throwError, ExceptT)
import qualified Data.ByteString as B
import Data.Traversable (traverse)
import Data.List (union, notElem, nub, sort)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vec
import System.Process (readProcess)
import qualified Data.Text as T
import Control.Lens (_2, view, over, set, firstOf, folded, Lens')

import qualified Notmuch
import Notmuch.Search
import Notmuch.Util (bracketT)

import Error
import Types


-- | apply tag operations on all given mails and write the resulting tags to the
-- database
messageTagModify
  :: (Traversable t, MonadError Error m, MonadIO m)
  => FilePath  -- ^ database
  -> [TagOp]
  -> t (a, NotmuchMail)
  -> m (t (a, NotmuchMail))
messageTagModify dbpath ops xs =
    withDatabase dbpath (\db -> traverse (applyTags ops db) xs)

applyTags
    :: (MonadError Error m, MonadIO m)
    => [TagOp]
    -> Notmuch.Database Notmuch.RW
    -> (a, NotmuchMail)
    -> m (a, NotmuchMail)
applyTags ops db mail = do
    let
      mail' = over _2 (tagItem ops) mail
      nmtags = view (_2 . mailTags) mail'
    if haveTagsChanged mail mail'
        then do
            tagsToMessage nmtags (view (_2 . mailId) mail') db
            pure mail'
        else pure mail'

tagItem :: ManageTags a => [TagOp] -> a -> a
tagItem ops mail = foldl (flip applyTagOp) mail ops

haveTagsChanged :: (a, NotmuchMail) -> (a, NotmuchMail) -> Bool
haveTagsChanged m1 m2 = sort (nub (view (_2 . mailTags) m1)) /= sort (nub (view (_2 . mailTags) m2))

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

getTags :: (ManageTags a) => a -> [Tag]
getTags = view tags

hasTag :: (ManageTags a) => Tag -> a -> Bool
hasTag t x = t `elem` view tags x

instance ManageTags NotmuchMail where
  tags = mailTags

instance ManageTags NotmuchThread where
  tags = thTags

-- | A helper function for opening, performing work,
-- and closing the database
withDatabase
  :: (Notmuch.AsNotmuchError e, Notmuch.Mode a, MonadError e m, MonadIO m)
  => FilePath
  -> (Notmuch.Database a -> ExceptT e IO c)
  -> m c
withDatabase dbpath = bracketT (Notmuch.databaseOpen dbpath) Notmuch.databaseDestroy

-- | Same as 'withDatabase', but the database connection
-- is read-only
withDatabaseReadOnly
  :: (Notmuch.AsNotmuchError e, MonadError e m, MonadIO m)
  => FilePath
  -> (Notmuch.Database Notmuch.RO -> ExceptT e IO c)
  -> m c
withDatabaseReadOnly = withDatabase

-- | creates a vector of parsed mails from a not much search
-- Note, that at this point in time only free form searches are supported. Also,
-- we filter out the tag which we use to mark mails as new mails
getMessages
  :: (MonadError Error m, MonadIO m)
  => T.Text
  -> NotmuchSettings FilePath
  -> m (Vec.Vector NotmuchMail)
getMessages s settings =
  withDatabaseReadOnly (view nmDatabase settings) go
  where go db = do
              msgs <- Notmuch.query db (FreeForm $ T.unpack s) >>= Notmuch.messages
              mails <- liftIO $ mapM messageToMail msgs
              pure $ Vec.fromList mails

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
  :: (MonadError Error m, MonadIO m)
  => T.Text
  -> NotmuchSettings FilePath
  -> m (Vec.Vector NotmuchThread)
getThreads s settings =
  withDatabaseReadOnly (view nmDatabase settings) go
  where
    go db = do
        ts <- Notmuch.query db (FreeForm $ T.unpack s) >>= Notmuch.threads
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
  withDatabaseReadOnly fp go
  where go db = do
          msgs <- getThread db (view thId t) >>= Notmuch.messages
          mails <- liftIO $ traverse messageToMail msgs
          pure $ Vec.fromList mails

-- | retrieve a given thread from the notmuch database by it's id
-- Note: The notmuch API does not provide a designated endpoint for retrieving
-- the thread by it's ID. We're cheating here by simply querying for the given
-- thread id.
--
getThread
  :: (MonadError Error m, MonadIO m)
  => Notmuch.Database mode -> B.ByteString -> m (Notmuch.Thread mode)
getThread db tid = do
  t <- Notmuch.query db (Thread tid) >>= Notmuch.threads
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

reloadThreadTags
    :: (MonadError Error m, MonadIO m)
    => FilePath -> NotmuchThread -> m NotmuchThread
reloadThreadTags fp item = withDatabaseReadOnly fp go
  where
    go db = fmap (`setTags` item) . Notmuch.tags =<< getThread db (view thId item)

fixupWhitespace :: T.Text -> T.Text
fixupWhitespace = T.map f . T.filter (/= '\n')
  where f '\t' = ' '
        f c = c
