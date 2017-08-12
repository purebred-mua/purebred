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
import UI.Types (NotmuchSettings, nmDatabase, nmNewTag)
import Control.Lens.Getter (view)
import Control.Lens.Setter (set)


getMessages :: T.Text -> NotmuchSettings -> IO (Vec.Vector Mail)
getMessages s settings = do
  db' <- databaseOpen (view nmDatabase settings)
  case db' of
    Left status -> do
        error $ show status
    Right db -> do
        q <- query db (FreeForm $ T.unpack s)
        msgs <- messages q
        mails <- mapM messageToMail msgs
        return $ Vec.fromList $ isNewMail (view nmNewTag settings) <$> mails

messageToMail
    :: HasTags Message
    => Message
    -> IO Mail
messageToMail m =
    Mail <$>
    (decodeUtf8 . fromMaybe "" <$> messageHeader "Subject" m) <*>
    (decodeUtf8 . fromMaybe "" <$> messageHeader "To" m) <*>
    (decodeUtf8 . fromMaybe "" <$> messageHeader "From" m) <*>
    messageFilename m <*>
    tagsToText m <*>
    pure False

tagsToText :: HasTags a => a -> IO [T.Text]
tagsToText m = do
  t <- tags m
  pure $ decodeUtf8 <$> t

getDatabasePath :: IO (FilePath)
getDatabasePath = getFromNotmuchConfig "database.path"

getFromNotmuchConfig :: String -> IO String
getFromNotmuchConfig key = do
  let cmd = "notmuch"
  let args = ["config", "get", key]
  stdout <- readProcess cmd args []
  pure $ filter (/= '\n') stdout

isNewMail :: T.Text -> Mail -> Mail
isNewMail t m = let nm = t `elem` (view mailTags m)
                in set mailIsNew nm m
