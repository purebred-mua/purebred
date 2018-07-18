{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | module to integrate with a mail parser. This is needed to actually view the
-- entire mail and it's attachments.
module Storage.ParsedMail where

import Control.Exception (try)
import Control.Lens (firstOf)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T

import Data.MIME

import Storage.Notmuch (mailFilepath)
import Types (Error(..), NotmuchMail, decodeLenient)

parseMail
  :: (MonadError Error m, MonadIO m)
  => NotmuchMail -> FilePath -> m MIMEMessage
parseMail m dbpath = do
  filePath <- mailFilepath m dbpath
  liftIO (try (B.readFile filePath))
    >>= either (throwError . FileReadError filePath) pure
    >>= either (throwError . FileParseError filePath) pure
        . parse (message mime)

getHeader :: CI.CI B.ByteString -> Message s a -> T.Text
getHeader k =
  maybe "header not found" decodeLenient
  . firstOf (headers . header k)

getFrom :: Message s a -> T.Text
getFrom = getHeader "from"

getSubject :: Message s a -> T.Text
getSubject = getHeader "subject"

getTo :: Message s a -> T.Text
getTo = getHeader "to"
