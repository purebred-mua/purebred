-- | module to integrate with a mail parser. This is needed to actually view the
-- entire mail and it's attachments.
{-# LANGUAGE OverloadedStrings #-}
module Storage.ParsedMail where

import           Codec.MIME.Parse    (parseMIMEMessage)
import           Codec.MIME.Type     (MIMEParam(..), MIMEValue(..), mime_val_headers)
import           Control.Exception   (try)
import           Control.Lens.Getter ((^.))
import qualified Data.Text           as T
import           Data.Text.IO        (readFile)
import           Prelude             hiding (readFile)
import           Storage.Mail        (Mail, filepath)
import Types (ParsedMail(..))

parseMail :: Mail -> IO (Either String ParsedMail)
parseMail m = do
  msg <- try (readFile $ m^.filepath) :: IO (Either IOError T.Text)
  case msg of
    Left e -> pure $ Left $ show e
    Right contents -> pure $ Right $ MIMEMail (parseMIMEMessage contents)

getFrom :: ParsedMail -> T.Text
getFrom (MIMEMail v) = findHeader v "from"
getFrom _ = throwNotImplemented

getSubject :: ParsedMail -> T.Text
getSubject (MIMEMail v) = findHeader v "subject"
getSubject _ = throwNotImplemented

getTo :: ParsedMail -> T.Text
getTo (MIMEMail v) = findHeader v "to"
getTo _ = throwNotImplemented

throwNotImplemented :: a
throwNotImplemented = error "Not implemented. ParsedMail.hs needs a proper mail parser"

findHeader :: MIMEValue -> T.Text -> T.Text
findHeader m name = T.strip . paramValue . head $ filter (\x -> paramName x == name) $ mime_val_headers m
