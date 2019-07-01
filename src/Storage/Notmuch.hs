{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | module for integrating notmuch within purebred
module Storage.Notmuch where

import Control.Monad ((>=>), when)
import Data.Function (on)
import System.IO.Unsafe (unsafeInterleaveIO)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (MonadError, throwError, ExceptT, runExceptT)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Traversable (traverse)
import Data.List (union, notElem, nub, sort)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vec
import System.Exit (ExitCode(..))
import qualified Data.Text as T
import Control.Lens (view, over, set, firstOf, folded, Lens')

import qualified Notmuch

import Error
import Types
import Purebred.LazyVector
import Purebred.System.Process (readProcess, proc)
import Purebred.Types.IFC (sanitiseText, untaint)


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

getTags :: (ManageTags a) => a -> [Tag]
getTags = view tags

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
              msgs <- Notmuch.query db (Notmuch.Bare $ T.unpack s) >>= Notmuch.messages
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
getDatabasePath = do
  let cmd = "notmuch"
  let args = ["config", "get", "database.path"]
  (exitc, stdout, err) <- readProcess $ proc cmd args
  case exitc of
    ExitFailure _ -> error (untaint decode err)
    ExitSuccess -> pure (filter (/= '\n') (untaint decode stdout))
  where
      decode = T.unpack . sanitiseText . decodeLenient . LB.toStrict

-- | creates a vector of threads from a notmuch search
--
getThreads
  :: (MonadError Error m, MonadIO m)
  => T.Text
  -> NotmuchSettings FilePath
  -> m (V NotmuchThread)
getThreads s settings =
  withDatabaseReadOnly (view nmDatabase settings) $
    flip Notmuch.query (Notmuch.Bare $ T.unpack s)
    >=> Notmuch.threads
    >=> liftIO . fmap (fromList 128) . lazyTraverse threadToThread

lazyTraverse :: (a -> IO b) -> [a] -> IO [b]
lazyTraverse f =
  foldr (\x ys -> (:) <$> f x <*> unsafeInterleaveIO ys) (pure [])

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
