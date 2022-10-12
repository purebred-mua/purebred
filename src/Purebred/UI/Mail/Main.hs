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

import Control.Lens
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI

import Brick
  ( Location(..), Padding(..), ViewportType(..), Widget
  , (<+>), (<=>), cached, hBox, padLeftRight, padRight, padTop
  , txt, txtWrap, vBox, viewport, visibleRegion, withAttr
  )
import qualified Brick.Widgets.List as L

import Data.MIME

import Purebred.Storage.Mail (takeFileName)
import Purebred.Types
import Purebred.Types.Presentation (MatchAtLine(..), widget)
import Purebred.UI.Attr
  ( headerKeyAttr, headerValueAttr, mailViewAttr, listSelectedAttr
  , listAttr)
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
    (matchLine, bod) =
      widget (view (asMailView . mvBody) s) (fromIntegral curMatchIndex)
    body' = cached ScrollingMailView $ case matchLine of
      NoMatch -> bod
      MatchAtLine i -> visibleRegion (Location (0, fromIntegral i)) (1, 1) bod

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
      widg = hBox
        [ padLeftRight 1 attachmentType
        , padRight Max (txt pFilename)
        , txt pType
        ]
  in withAttr listItemAttr widg
