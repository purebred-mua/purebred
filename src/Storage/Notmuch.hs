{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | module for integrating notmuch within purebred
module Storage.Notmuch where

import Types (NotmuchMail(..))
import Notmuch
import Notmuch.Search

import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Data.Foldable (traverse_)
import Data.Functor (($>))
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
  bracket (runExceptT (databaseOpenReadOnly (view nmDatabase settings)))
          (runExceptT . traverse_ databaseDestroy)
          (runExceptT . (either throwError pure >=> go))
  where go db = do
              msgs <- query db (FreeForm $ T.unpack s) >>= messages
              mails <- liftIO $ mapM messageToMail msgs
              pure $ Vec.fromList mails

setNotmuchMailTags
    :: FilePath
    -> NotmuchMail
    -> IO (Either String NotmuchMail)
setNotmuchMailTags dbpath m =
  case mailTagsToNotmuchTags m of
    Nothing -> pure $ Left "Tags are corrupt"
    Just nmtags ->
      bracket (runExceptT (databaseOpen dbpath))
              (runExceptT . traverse_ databaseDestroy)
              (runExceptT . (either (throwError . show) pure >=> tagsToMessage nmtags m))

tagsToMessage
  :: (MonadError String m, MonadIO m)
  => [Tag] -> NotmuchMail -> Database RW -> m NotmuchMail
tagsToMessage xs m db =
  -- This is nasty.  We have to run the transformer so we can change the
  -- error type.  Instead, we should update hs-notmuch to abstract over
  -- an "AsNotmuchError" typeclass, then create our own error sum type
  -- with an instance of it.
  runExceptT (
    findMessage db (view mailId m)
    >>= traverse (\msg -> messageSetTags xs msg $> m)
  ) >>= either (throwError . show) (maybe (throwError "boop") pure)

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
