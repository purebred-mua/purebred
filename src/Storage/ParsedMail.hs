{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | module to integrate with a mail parser. This is needed to actually view the
-- entire mail and it's attachments.
module Storage.ParsedMail where

import Codec.MIME.Parse (parseMIMEMessage)
import Codec.MIME.Type
       (MIMEParam(..), MIMEValue(..), mime_val_headers)
import Control.Exception (try)
import Control.Lens (firstOf)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Storage.Notmuch (mailFilepath)

import Data.MIME

import Error
import Types (ParsedMail(..), NotmuchMail, decodeLenient)

parseMail
  :: (MonadError Error m, MonadIO m)
  => NotmuchMail -> FilePath -> m ParsedMail
parseMail m dbpath = do
  filePath <- mailFilepath m dbpath
  {-
  liftIO (try (T.readFile filePath))
    >>= either
      (throwError . FileReadError filePath)
      (pure . MIMEMail . parseMIMEMessage)
      -}
  liftIO (try (L.readFile filePath))
    >>= either (throwError . FileReadError filePath) pure
    >>= either (throwError . FileParseError filePath) (pure . PurebredEmail)
        . parse (message mime)

getHeader :: CI.CI B.ByteString -> Message a -> T.Text
getHeader k =
  maybe "header not found" decodeLenient
  . firstOf (messageHeaders . header k)

getFrom :: ParsedMail -> T.Text
getFrom (MIMEMail v) = findHeader v "from"
getFrom (PurebredEmail msg) = getHeader "from" msg

getSubject :: ParsedMail -> T.Text
getSubject (MIMEMail v) = findHeader v "subject"
getSubject (PurebredEmail msg) = getHeader "subject" msg

getTo :: ParsedMail -> T.Text
getTo (MIMEMail v) = findHeader v "to"
getTo (PurebredEmail msg) = getHeader "to" msg

findHeader :: MIMEValue -> T.Text -> T.Text
findHeader m name = T.strip . paramValue . head $ filter (\x -> paramName x == name) $ mime_val_headers m
