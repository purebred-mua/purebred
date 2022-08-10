-- This file is part of purebred
-- Copyright (C) 2017-2021 Róman Joost
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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Module holding generic widgets.
module Purebred.UI.Draw.Main
  ( fillLine
  , renderEditorWithLabel
  , attachmentsHeader
  ) where

import Brick.Types (Widget)
import Brick.Widgets.Core
  (Padding(..), fill, txt, vLimit, padRight, (<+>), withAttr, padLeft, hBox)
import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import Data.Proxy
import Control.Lens (view)
import Purebred.Types
import Purebred.UI.Views (focusedViewWidget)
import Purebred.UI.Actions (HasName(..), HasEditor(..))
import Purebred.Config
  (editorLabelAttr, editorAttr, editorFocusedAttr, statusbarAttr, editorErrorAttr)
import Purebred.UI.Notifications (hasError)

-- | Fills the entire line with spaces. This can be used to draw a
-- visual bar when an 'AttrName' with a background colour is set.
--
fillLine :: Widget Name
fillLine = vLimit 1 (fill ' ')

attachmentsHeader :: Widget Name
attachmentsHeader = withAttr statusbarAttr $ hBox [ padLeft (Pad 1) (txt "-- Attachments") , vLimit 1 (fill '-')]

editorDrawContent :: Bool -> [T.Text] -> Widget Name
editorDrawContent showError st = let widget = txt $ T.unlines st
                                 in if showError then withAttr editorErrorAttr widget else widget

-- | Renders editor with a label on the left restricted to one line
renderEditorWithLabel
  :: forall n. (HasName n, HasEditor n)
  => Proxy n -> T.Text -> AppState -> Widget Name
renderEditorWithLabel _ label s =
  let hasFocus = name @n == focusedViewWidget s
      inputW = E.renderEditor (editorDrawContent (hasError s)) hasFocus (view (editorL @n) s)
      labelW = withAttr editorLabelAttr $ padRight (Pad 1) $ txt label
      eAttr =
        if hasFocus
          then editorFocusedAttr
          else editorAttr
   in labelW <+> withAttr eAttr (vLimit 1 inputW)
