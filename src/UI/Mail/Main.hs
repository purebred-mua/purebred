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
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Mail.Main
  ( renderMailView
  , renderAttachmentsList
  , renderPart
  , buildWordMarkup
  ) where

import qualified Brick.AttrMap as A
import Brick.Types (Padding(..), ViewportType(..), Widget)
import qualified Brick.Widgets.List as L
import Brick.Widgets.Core
  (padTop, padBottom, txt, txtWrap, viewport, (<+>), (<=>), withAttr,
   vBox, hBox, padLeftRight, padRight)
import Brick.Markup (markup, (@?))
import Brick.Focus (focusGetCurrent)
import Data.Text.Markup (Markup, markupSet)

import Control.Lens
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import Prelude hiding (Word)

import Data.MIME

import Types
import UI.Draw.Main (attachmentsHeader)
import UI.Views (focusedViewWidget)
import Purebred.Config
  (headerKeyAttr, headerValueAttr, mailViewAttr, listSelectedAttr,
   listAttr, textMatchHighlightAttr, currentTextMatchHighlightAttr,
   defaultAttr, mailbodySourceAttr)
import Purebred.Storage.Mail (takeFileName)

-- | Instead of using the entire rendering area to show the email, we still show
-- the index in context above the mail.
--
-- Implementation detail: Currently we're creating the sub list of mails we show
-- for each key press. This might have to change in the future.
renderMailView :: AppState -> Widget Name
renderMailView s = viewport ScrollingMailView Vertical (mailView s (view (asMailView . mvMail) s))

mailView :: AppState -> Maybe MIMEMessage -> Widget Name
mailView s (Just msg) = withAttr mailViewAttr $ messageToMailView s msg
mailView _ Nothing = txt "Eeek: this is not supposed to happen"

messageToMailView :: AppState -> MIMEMessage -> Widget Name
messageToMailView s msg =
  let
    body' = renderMarkup
      (preview (asMailView . mvScrollSteps . to focusGetCurrent . _Just) s)
      (view (asMailView . mvBody) s)

    wantHeader :: CI.CI B.ByteString -> Bool
    wantHeader = case view (asMailView . mvHeadersState) s of
      Filtered -> view (asConfig . confMailView . mvHeadersToShow) s
      ShowAll -> const True

    filteredHeaders =
      toListOf (headerList . folded . filtered (wantHeader . fst)) msg

    headerToWidget :: (CI.CI B.ByteString, B.ByteString) -> Widget Name
    headerToWidget (k, v) =
      withAttr headerKeyAttr $
        txt (decodeLenient (CI.original k) <> ": ")
        <+> withAttr headerValueAttr (txtWrap (decodeEncodedWords charsets v))

    headerWidgets = headerToWidget <$> filteredHeaders
    bodyWidget = padTop (Pad 1) body'
    charsets = view (asConfig . confCharsets) s
  in
    vBox headerWidgets <=> padTop (Pad 1) bodyWidget

renderAttachmentsList :: AppState -> Widget Name
renderAttachmentsList s =
    let hasFocus = MailListOfAttachments == focusedViewWidget s
        attachmentsList =
          L.renderList (\isSel -> renderPart charsets isSel . view headers) hasFocus (view (asMailView . mvAttachments) s)
        charsets = view (asConfig . confCharsets) s
    in attachmentsHeader <=> attachmentsList

renderPart :: CharsetLookup -> Bool -> Headers -> Widget Name
renderPart charsets selected hds =
  let pType = showContentType $ view contentType hds
      pFilename = maybe "--" takeFileName $
        preview (contentDisposition . folded . filename charsets) hds
      listItemAttr = if selected then listSelectedAttr else listAttr
      attachmentType = txt (if isAttachment hds then "A" else "I")
      widget = hBox
        [ padLeftRight 1 attachmentType
        , padRight Max (txt pFilename)
        , txt pType
        ]
  in withAttr listItemAttr widget

-- | render the Mailbody AST to a list used for Markup in Brick
--
renderMarkup ::  Maybe ScrollStep -> MailBody -> Widget Name
renderMarkup st b =
  let source =
        withAttr mailbodySourceAttr $
        padBottom (Pad 1) $ txt ("Showing output from: " <> view mbSource b)
      bodyMarkup = toListOf (mbParagraph . to (padBottom (Pad 1) . buildParagraph st)) b
   in source <=> vBox bodyMarkup

buildParagraph :: Maybe ScrollStep -> Paragraph -> Widget Name
buildParagraph st = vBox . toListOf (pLine . to (markup . buildWordMarkup st))

-- | Render the line by inserting markup if we have a match *and* a
-- scroll step matching
-- Note: Why are we ignoring the line number here? Because it only
-- matters for scrolling, not for highlighting the match.
--
buildWordMarkup :: Maybe ScrollStep -> Line -> Markup A.AttrName
buildWordMarkup st (Line xs _ t) = foldr (go st) (t @? defaultAttr) xs
  where
    go :: Maybe ScrollStep -> Match -> Markup A.AttrName -> Markup A.AttrName
    go Nothing (Match offset l _) m =
      markupSet (offset, l) textMatchHighlightAttr m
    go (Just step) ma@(Match offset l _) m =
      if view stMatch step == ma
        then markupSet (offset, l) currentTextMatchHighlightAttr m
        else markupSet (offset, l) textMatchHighlightAttr m
