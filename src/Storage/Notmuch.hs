-- | module for integrating notmuch within purebred
module Storage.Notmuch where

import           Storage.Mail

import           Notmuch
import           Notmuch.Search

import           Data.Foldable  (toList)
import           Data.Maybe     (fromMaybe)
import qualified Data.Vector    as Vec
import           System.Process (readProcess)


getMessages :: String -> String -> IO (Vec.Vector Mail)
getMessages dbfp searchterms = do
  db' <- databaseOpen dbfp
  case db' of
    Left status -> do
        error $ show status
    Right db -> do
        q <- query db (FreeForm searchterms)
        msgs <- messages q
        mails <- mapM messageToMail msgs
        return $ Vec.fromList $ toList mails

messageToMail :: Message -> IO Mail
messageToMail m = do
  s <- messageHeader "Subject" m
  t <- messageHeader "To" m
  f <- messageHeader "From" m
  fn <- messageFilename m
  pure $ Mail (fromMaybe "" s) (fromMaybe "" t) (fromMaybe "" f) fn

getDatabasePath :: IO (FilePath)
getDatabasePath = do
  let cmd = "notmuch"
  let args = ["config", "get", "database.path"]
  stdout <- readProcess cmd args []
  pure $ filter (/= '\n') stdout

