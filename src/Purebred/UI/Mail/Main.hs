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

module Purebred.UI.Mail.Main
  ( renderMailView
  , renderAttachmentsList
  , renderPart
  ) where

import qualified Brick.AttrMap as A
import Brick.Types (ViewportType(..), Widget)
import qualified Brick.Widgets.List as L
import Brick.Widgets.Core
  (Padding(..), padTop, padBottom, txt, txtWrap, viewport, (<+>), (<=>), withAttr,
   vBox, hBox, padLeftRight, padRight)
import Data.Text.Markup (Markup, markupSet)

import Control.Lens
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import Data.Tuple (swap)

import Data.MIME

import Purebred.Brick.Markup (markup, (@?))
import Purebred.Storage.Mail (takeFileName)
import Purebred.Types
import Purebred.UI.Attr
  (headerKeyAttr, headerValueAttr, mailViewAttr, listSelectedAttr,
   listAttr, textMatchHighlightAttr, currentTextMatchHighlightAttr,
   defaultAttr, mailbodySourceAttr)
import Purebred.UI.Draw.Main (attachmentsHeader)
import Purebred.UI.Views (focusedViewWidget)

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
    curMatchIndex = view (asMailView . mvSearchIndex) s
    curMatch = preview (asMailView . mvBody . mbMatches . ix curMatchIndex) s
    body' = renderMarkup curMatch (view (asMailView . mvBody) s)

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
renderMarkup :: Maybe Match -> MailBody -> Widget Name
renderMarkup cur b = source <=> vBox (markup <$> markups)
  where
  source =
    withAttr mailbodySourceAttr $
    padBottom (Pad 1) $ txt ("Showing output from: " <> view mbSource b)

  markups = markupLines (view mbMatches b) (zip [0..] (toListOf mbLines b))

  markupLines :: [Match] -> [(Int, T.Text)] -> [Markup A.AttrName]
  markupLines _  []     = []
  markupLines ms (s:ss) =
    let (ms', r) = markupLine s ms in r : markupLines ms' ss

  markupLine :: (Int, T.Text) -> [Match] -> ([Match], Markup A.AttrName)
  markupLine (i, s) =
    fmap (highlightLine s) . swap . span (\(Match _ _ line) -> line == i)

  highlightLine :: T.Text -> [Match] -> Markup A.AttrName
  highlightLine = foldr acc . (@? defaultAttr)
    where acc m@(Match off len _line) = markupSet (off, len) (attr m)

  attr m
    | Just m == cur = currentTextMatchHighlightAttr
    | otherwise     = textMatchHighlightAttr
