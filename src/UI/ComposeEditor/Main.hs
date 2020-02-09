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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module UI.ComposeEditor.Main
  ( attachmentsEditor
  , renderPart
  , drawHeaders
  , renderConfirm
  ) where

import Brick.Types (Padding(..), Widget)
import Brick.Widgets.Core
  ((<+>), (<=>), emptyWidget, hLimit, padBottom, padLeft,
   txt, vLimit)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Brick.Widgets.Dialog (renderDialog)
import Brick.Widgets.Center (hCenter)
import Control.Lens (to, view)
import qualified Data.Text as T
import Data.Text.Zipper (currentLine)

import Data.MIME (headers)

import UI.Views (focusedViewWidget)
import UI.Draw.Main (attachmentsHeader)
import UI.Mail.Main (renderPart)
import Types

attachmentsEditor :: AppState -> Widget Name
attachmentsEditor s =
    let hasFocus = ComposeListOfAttachments == focusedViewWidget s
        attachmentsList =
          L.renderList (\isSel -> renderPart charsets isSel . view headers) hasFocus (view (asCompose . cAttachments) s)
        charsets = view (asConfig . confCharsets) s
    in attachmentsHeader <=> attachmentsList

drawHeaders :: AppState -> Widget Name
drawHeaders s = 
  let headers' = [ComposeSubject, ComposeBcc, ComposeCc, ComposeTo, ComposeFrom]
  in padBottom (Pad 1) $ foldr (drawTableRows s) (txt T.empty) headers'

-- | align labels to the right and values to the left, e.g.
--
--     Foo: bar
-- Subject: test
--
drawTableRows :: AppState -> Name -> Widget Name -> Widget Name
drawTableRows s name w = w
                         <=> vLimit 1
                         (hLimit 15 (padLeft Max (makeLabel name))
                          <+> padLeft (Pad 1) (txt (widgetValue name s)))

makeLabel :: Name -> Widget Name
makeLabel ComposeFrom = txt "From:"
makeLabel ComposeTo = txt "To:"
makeLabel ComposeCc = txt "Cc:"
makeLabel ComposeBcc = txt "Bcc:"
makeLabel _ = txt "Subject:"

widgetValue :: Name -> AppState -> T.Text
widgetValue ComposeFrom = view (asCompose . cFrom . E.editContentsL . to currentLine)
widgetValue ComposeTo = view (asCompose . cTo . E.editContentsL . to currentLine)
widgetValue ComposeCc = view (asCompose . cCc . E.editContentsL . to currentLine)
widgetValue ComposeBcc = view (asCompose . cBcc . E.editContentsL . to currentLine)
widgetValue ComposeSubject = view (asCompose . cSubject . E.editContentsL . to currentLine)
widgetValue _ = const T.empty

renderConfirm :: AppState -> Widget Name
renderConfirm s = renderDialog (view (asCompose . cKeepDraft) s) $ hCenter emptyWidget
