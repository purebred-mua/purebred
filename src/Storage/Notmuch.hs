{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | module for integrating notmuch within purebred
module Storage.Notmuch where

import           Storage.Mail
import           Notmuch
import           Notmuch.Search

import           Data.Maybe     (fromMaybe)
import qualified Data.Vector    as Vec
import           System.Process (readProcess)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Types (NotmuchSettings, nmDatabase, nmNewTag)
import Control.Lens.Getter (view)


-- | creates a vector of parsed mails from a not much search
-- Note, that at this point in time only free form searches are supported. Also,
-- we filter out the tag which we use to mark mails as new mails
getMessages :: T.Text -> NotmuchSettings -> IO (Vec.Vector Mail)
getMessages s settings = do
  db' <- databaseOpen (view nmDatabase settings)
  case db' of
    Left status -> do
        error $ show status
    Right db -> do
        q <- query db (FreeForm $ T.unpack s)
        msgs <- messages q
        mails <- mapM (messageToMail $ view nmNewTag settings) msgs
        return $ Vec.fromList mails

messageToMail
    :: HasTags Message
    => T.Text
    -> Message
    -> IO Mail
messageToMail ignoredTag m =
    Mail <$>
    (decodeUtf8 . fromMaybe "" <$> messageHeader "Subject" m) <*>
    (decodeUtf8 . fromMaybe "" <$> messageHeader "To" m) <*>
    (decodeUtf8 . fromMaybe "" <$> messageHeader "From" m) <*>
    messageFilename m <*>
    messageDate m <*>
    (tagsToText m ignoredTag) <*>
    isNewMail m ignoredTag

tagsToText :: HasTags a => a -> T.Text -> IO [T.Text]
tagsToText m ignored = do
  t <- tags m
  pure $ filter (/= ignored) $ decodeUtf8 <$> t

getDatabasePath :: IO (FilePath)
getDatabasePath = getFromNotmuchConfig "database.path"

getFromNotmuchConfig :: String -> IO String
getFromNotmuchConfig key = do
  let cmd = "notmuch"
  let args = ["config", "get", key]
  stdout <- readProcess cmd args []
  pure $ filter (/= '\n') stdout

isNewMail :: HasTags Message => Message -> T.Text -> IO Bool
isNewMail m newTag = do
  t <- tags m
  pure $ newTag `elem` (decodeUtf8 <$> t)
