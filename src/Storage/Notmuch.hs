{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | module for integrating notmuch within purebred
module Storage.Notmuch where

import Types (NotmuchMail(..))
import Notmuch
import Notmuch.Search

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (runExceptT)
import Data.Traversable (traverse)
import Data.List (union)
import Data.Maybe (fromMaybe)
import Data.Bifunctor (first)
import qualified Data.Vector as Vec
import System.Process (readProcess)
import Control.Exception (bracket)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Types (NotmuchSettings, nmDatabase, mailId, mailTags)
import Control.Lens.Getter (view)
import Control.Lens.Setter (over)


-- | creates a vector of parsed mails from a not much search
-- Note, that at this point in time only free form searches are supported. Also,
-- we filter out the tag which we use to mark mails as new mails
getMessages :: T.Text
            -> NotmuchSettings FilePath
            -> IO (Either String (Vec.Vector NotmuchMail))
getMessages s settings =
  first show <$>
  bracket (runExceptT (databaseOpenReadOnly (view nmDatabase settings))
           >>= either (error . show) pure)
          (void . runExceptT . databaseDestroy)
          (\db -> runExceptT $ do
              msgs <- query db (FreeForm $ T.unpack s) >>= messages
              mails <- liftIO $ mapM messageToMail msgs
              return $ Vec.fromList mails)

setNotmuchMailTags
    :: FilePath
    -> NotmuchMail
    -> IO (Either String NotmuchMail)
setNotmuchMailTags dbpath m =
  case mailTagsToNotmuchTags m of
    Nothing -> pure $ Left "Tags are corrupt"
    Just nmtags ->
      bracket (runExceptT (databaseOpen dbpath) >>= either (error . show) pure)
              (void . runExceptT . databaseDestroy)
              (\db -> tagsToMessage nmtags m db)

tagsToMessage
  :: [Tag] -> NotmuchMail -> Database RW -> IO (Either String NotmuchMail)
tagsToMessage xs m db =
  runExceptT
    (findMessage db
                 (view mailId m)) >>=
  \case
    Left e -> pure $ Left (show e)
    Right Nothing -> pure $ Left "boop"
    Right (Just msg) -> do
      _ <- runExceptT (messageSetTags xs msg)
      pure $ Right m

addTag :: NotmuchMail -> T.Text -> NotmuchMail
addTag m t = over mailTags (`union` [t]) m

removeTag :: NotmuchMail -> T.Text -> NotmuchMail
removeTag m t = over mailTags (filter (/= t)) m

mailTagsToNotmuchTags :: NotmuchMail -> Maybe [Tag]
mailTagsToNotmuchTags m =
    let xs = view mailTags m
    in traverse id (mkTag . encodeUtf8 <$> xs)

messageToMail
    :: HasTags (Message n a)
    => Message n a
    -> IO NotmuchMail
messageToMail m = do
    tgs <- tags m
    let tgs' = decodeUtf8 . getTag <$> tgs
    NotmuchMail <$>
      (decodeUtf8 . fromMaybe "" <$> messageHeader "Subject" m) <*>
      (decodeUtf8 . fromMaybe "" <$> messageHeader "From" m) <*>
      messageFilename m <*>
      messageDate m <*>
      pure tgs' <*>
      messageId m

getDatabasePath :: IO (FilePath)
getDatabasePath = getFromNotmuchConfig "database.path"

getFromNotmuchConfig :: String -> IO String
getFromNotmuchConfig key = do
  let cmd = "notmuch"
  let args = ["config", "get", key]
  stdout <- readProcess cmd args []
  pure $ filter (/= '\n') stdout

mailIsNew :: T.Text -> NotmuchMail -> Bool
mailIsNew ignoredTag m = ignoredTag `elem` (view mailTags m)
