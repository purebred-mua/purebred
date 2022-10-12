-- This file is part of purebred
-- Copyright (C) 2017-2019 Róman Joost and Fraser Tweedale
--
-- purebred is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Purebred.Storage.Mail (
  -- * Synopsis
  -- $synopsis

  -- * API
    parseMail
  , parseMailbody
  , bodyToDisplay

  -- ** Header data
  , toQuotedMail
  , takeFileName

  -- ** Attachment handling
  , toMIMEMessage
  , chooseEntity
  , entityToText
  , entityToBytes
  , writeEntityToPath
  ) where

import Control.Applicative ((<|>))
import Control.Exception (try)
import Control.Lens
import Data.Text.Lens (packed)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadMask)
import Data.Foldable (toList)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified System.FilePath as FP (takeFileName)

import Data.MIME

import Purebred.Types
import Purebred.System (tryIO)
import Purebred.Types.Error
import Purebred.Types.IFC (sanitiseText)
import Purebred.Types.Mailcap
  ( MailcapHandler, mhMakeProcess, mpCommand, hasCopiousoutput
  , mailcapHandlerToEntityCommand
  )
import Purebred.Types.Presentation (BodyPresentation(..))
import Purebred.Types.Presentation.MailBody (parseMailbody)
import Purebred.Storage.Client (Server, mailFilepath)
import Purebred.System.Process (outputToText, runEntityCommand)

{- $synopsis

This module integrates with an email parser in order to display all
parts.

-}

parseMail
  :: (MonadError Error m, MonadIO m)
  => NotmuchMail -> Server -> m MIMEMessage
parseMail m server = do
  filePath <- mailFilepath m server
  liftIO (try (B.readFile filePath))
    >>= either (throwError . FileReadError filePath) pure
    >>= either (throwError . FileParseError filePath) pure
        . parse (message mime)

bodyToDisplay ::
     (MonadMask m, MonadError Error m, MonadIO m)
  => AppState
  -> Int
  -> CharsetLookup
  -> ContentType
  -> MIMEMessage
  -> m (MIMEMessage, BodyPresentation)
bodyToDisplay s textwidth charsets prefCT msg =
  case chooseEntity prefCT msg of
    Nothing ->
      throwError
        (ParseError $ "Unable to find preferred entity with: " <> show prefCT)
    Just entity ->
      let output =
            maybe
              (pure $ parseMailbody textwidth "Internal Viewer" $ entityToText charsets entity)
              (\handler ->
                 parseMailbody textwidth (showHandler handler) <$>
                 entityPiped handler entity)
              (findAutoview s entity)
          showHandler = view (mhMakeProcess . mpCommand . to (T.pack . toList))
       in (msg, ) <$> output

findAutoview :: AppState -> WireEntity -> Maybe MailcapHandler
findAutoview s msg =
  let match ct = firstOf (asConfig . confMailView . mvMailcap . hasCopiousoutput . filtered (`fst` ct) . _2) s
  in match =<< preview (headers . contentType) msg


-- | Pick a preferred entity to be displayed in the UI.
--
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

-- | Render the entity to be written to the filesystem. In case of a
-- decoding error propagates an 'Error'.
--
entityToBytes :: (MonadError Error m) => WireEntity -> m B.ByteString
entityToBytes msg = either err pure (convert msg)
  where
    err e = throwError $ ParseError ("Decoding error: " <> show e)
    convert :: WireEntity -> Either EncodingError B.ByteString
    convert m = view body <$> view transferDecoded m

-- | Render the entity to be displayed in the UI. If decoding errors,
-- returns an error message instead.
--
entityToText :: CharsetLookup -> WireEntity -> T.Text
entityToText charsets msg = sanitiseText . either err (view body) $
  view transferDecoded msg >>= view (charsetDecoded charsets)
  where
    err :: EncodingError -> T.Text
    err e =
      "ERROR: " <> view (to show . packed) e <> ". Showing raw body.\n\n"
      <> decodeLenient (view body msg)

-- | Pipe an entity through the command given by the 'MailcapHandler'.
--
entityPiped ::
     (MonadMask m, MonadError Error m, MonadIO m)
  => MailcapHandler
  -> WireEntity
  -> m T.Text
entityPiped handler msg =
  entityToBytes msg
  >>= fmap outputToText . runEntityCommand . mailcapHandlerToEntityCommand handler

quoteText :: T.Text -> T.Text
quoteText = ("> " <>)

-- | Construct a reply to the given message, quoting the body.
toQuotedMail
  :: CharsetLookup
  -> ReplySettings
  -> BodyPresentation   -- ^ Body of message being replied to
  -> MIMEMessage -- ^ Message being replied to
  -> MIMEMessage
toQuotedMail charsets settings mbody msg =
  reply charsets settings msg
    & setTextPlainBody (T.unlines . fmap quoteText $ toTextLines mbody)

-- | Convert an entity into a MIMEMessage used, for example, when
-- re-composing a draft mail.
--
toMIMEMessage :: CharsetLookup -> WireEntity -> MIMEMessage
toMIMEMessage charsets m@(Message _ bs) =
  let ct = view (headers . contentType) m
      fp = preview (headers . contentDisposition . folded . filename charsets . to T.unpack) m
      cdType = preview (headers . contentDisposition . folded . dispositionType) m
  in case cdType of
    (Just Inline) -> createTextPlainMessage (entityToText charsets m)
    _ -> createAttachment ct fp bs

-- | Version of takeFileName handling 'Text' values
--
takeFileName :: T.Text -> T.Text
takeFileName = T.pack . FP.takeFileName . T.unpack

-- | Low-level function to save the 'WireEntity' to disk.
--
writeEntityToPath ::
     (MonadError Error m, MonadIO m) => FilePath -> WireEntity -> m FilePath
writeEntityToPath filepath entity = do
  entityToBytes entity >>= tryIO . B.writeFile filepath
  pure filepath
