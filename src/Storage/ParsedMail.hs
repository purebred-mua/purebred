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

module Storage.ParsedMail (
  -- * Synopsis
  -- $synopsis

  -- * API
    parseMail
  , bodyToDisplay
  , findMatchingWords
  , removeMatchingWords
  , makeScrollSteps

  -- ** Header data
  , getTo
  , getSubject
  , getForwardedSubject
  , getFrom
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
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified System.FilePath as FP (takeFileName)
import Prelude hiding (Word)

import Data.MIME

import Error
import UI.Notifications (makeWarning)
import Storage.Notmuch (mailFilepath)
import Types
import Purebred.System (tryIO)
import Purebred.Types.IFC (sanitiseText)
import Purebred.Parsing.Text (parseMailbody)
import Purebred.System.Process
  (runEntityCommand, tmpfileResource, toProcessConfigWithTempfile,
  tryReadProcessStdout, handleExitCodeThrow)

{- $synopsis

This module integrates with an email parser in order to display all
parts.

-}

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

-- | Returns the subject line formatted for forwarding.
--
getForwardedSubject ::
     Message s a -- ^ the encapsulated mail
  -> T.Text
getForwardedSubject m = "[" <> getFrom m <> ": " <> getSubject m <> "]"

-- | Create a list of steps to record which absolute positions
-- brick/the terminal should scroll.
makeScrollSteps :: MailBody -> [ScrollStep]
makeScrollSteps = mkScrollStep <$> itoListOf (indexing (mbParagraph . pLine . lMatches . traversed))
  where
    mkScrollStep :: [(Int, Match)] -> [ScrollStep]
    mkScrollStep = fmap (\(n, m) -> (n + 1, view mLinenumber m, m))

-- | Find matching words in the AST and change the annotation so
-- they're highlighted during rendering
--
-- Note, that the matching is case sensitive.
--
findMatchingWords :: T.Text -> MailBody -> MailBody
findMatchingWords     "" = removeMatchingWords
findMatchingWords needle =
  over (mbParagraph . pLine) go
  where
    go :: Line -> Line
    go line =
      let lengthNeedle = T.length needle
          lineNumber = view lNumber line
          allMatches =
            (\(h, _) -> Match (T.length h) lengthNeedle lineNumber) <$>
            T.breakOnAll needle (view lText line)
       in set lMatches allMatches line

-- | Reset all matching words, effectively removing any information
-- for highlights
--
removeMatchingWords :: MailBody -> MailBody
removeMatchingWords =
  set (mbParagraph . pLine . filtered hasMatches . lMatches) []

bodyToDisplay ::
     (MonadMask m, MonadError Error m, MonadIO m)
  => AppState
  -> Int
  -> CharsetLookup
  -> ContentType
  -> MIMEMessage
  -> m (MIMEMessage, MailBody)
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
  entityToBytes msg >>= mkConfig handler >>= runEntityCommand

-- | Create an entity command which writes our entity to a tempfile,
-- runs the command given by the 'MailcapHandler' over it and grab the
-- stdout for later display.
--
mkConfig ::
     (MonadError Error m, MonadIO m)
  => MailcapHandler
  -> B.ByteString
  -> m (EntityCommand m FilePath)
mkConfig cmd =
  pure .
  EntityCommand
    handleExitCodeThrow
    (tmpfileResource (view mhKeepTemp cmd))
    (\_ fp -> toProcessConfigWithTempfile (view mhMakeProcess cmd) fp)
    tryReadProcessStdout

quoteText :: T.Text -> T.Text
quoteText = ("> " <>)

-- | Creates a new instance of `MIMEMessage` with a quoted plain text part if:
-- a) the preferred content type can be extracted
-- b) the text entity can be successfully decoded
-- otherwise an empty plain text body is created
toQuotedMail
  :: [Mailbox]
  -> MailBody
  -> MIMEMessage
  -> MIMEMessage
toQuotedMail mailboxes mbody msg =
    let contents = T.unlines $ toListOf (mbParagraph . pLine . lText . to quoteText) mbody
        replyToAddress m =
            firstOf (headers . header "reply-to") m
            <|> firstOf (headers . header "from") m
    in createTextPlainMessage contents
                 & set (headers . at "from") (Just $ renderMailboxes mailboxes)
                 . set (headers . at "to") (replyToAddress msg)
                 . set (headers . at "references") (view (headers . replyHeaderReferences) msg)
                 . set (headers . at "subject") (("Re: " <>) <$> view (headers . at "subject") msg)

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
