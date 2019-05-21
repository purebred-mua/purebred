{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | module to integrate with a mail parser. This is needed to actually view the
-- entire mail and it's attachments.
module Storage.ParsedMail where

import Control.Applicative ((<|>))
import Control.Exception (try)
import Control.Lens
       (firstOf, view, preview, filtered, to, (&), set, preview, at)
import Data.Text.Lens (packed)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Semigroup ((<>))
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T

import Data.MIME

import Error
import Storage.Notmuch (mailFilepath)
import Types (NotmuchMail, decodeLenient)
import Purebred.Types.IFC (sanitiseText)

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

chooseEntity :: ContentType -> MIMEMessage -> Maybe WireEntity
chooseEntity preferredContentType msg =
  let
    match x = matchContentType
      (view (headers . contentType . ctType) x)
      (preview (headers . contentType . ctSubtype) x)
      preferredContentType

    -- select first entity with matching content-type;
    -- otherwise select first entity;
  in firstOf (entities . filtered match) msg <|> firstOf entities msg

entityToBytes :: (MonadError Error m) => WireEntity -> m B.ByteString
entityToBytes msg = either err pure (convert msg)
  where
    err e = throwError $ GenericError ("Decoding error: " <> show e)
    convert :: WireEntity -> Either EncodingError B.ByteString
    convert m = view body <$> view transferDecoded m

entityToText :: WireEntity -> T.Text
entityToText msg = sanitiseText . either err (view body) $
  view transferDecoded msg >>= view charsetDecoded
  where
    err :: EncodingError -> T.Text
    err e =
      "ERROR: " <> view (to show . packed) e <> ". Showing raw body.\n\n"
      <> decodeLenient (view body msg)

quoteText :: T.Text -> T.Text
quoteText = T.unlines . fmap ("> " <>) . T.lines

-- | Creates a new instance of `MIMEMessage` with a quoted plain text part if:
-- a) the preferred content type can be extracted
-- b) the text entity can be successfully decoded
-- otherwise an empty plain text body is created
toQuotedMail :: ContentType -> MIMEMessage -> Either Error MIMEMessage
toQuotedMail ct msg =
    let contents =
            case chooseEntity ct msg of
                Nothing ->
                    Left
                        (GenericError $
                         "Unable to find preferred content type: " <>
                         T.unpack (showContentType ct))
                Just ent -> Right $ quoteText (entityToText ent)
        replyToAddress m =
            firstOf (headers . header "reply-to") m
            <|> firstOf (headers . header "from") m
    in fmap
           (\x ->
                 createTextPlainMessage x
                 & set (headers . at "from") (view (headers . at "to") msg)
                 . set (headers . at "to") (replyToAddress msg)
                 . set (headers . at "references") (view (headers . replyHeaderReferences) msg)
                 . set (headers . at "subject") (("Re: " <>) <$> view (headers . at "subject") msg))
           contents
