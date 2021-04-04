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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
-- | Module holding generic widgets.
module UI.Draw.Main
  ( fillLine
  , renderEditorWithLabel
  , attachmentsHeader
  , renderHaskeline
  ) where

import Brick.Types (Padding(..), Widget)
import Brick.Widgets.Core
  (fill, txt, vLimit, padRight, (<+>), withAttr, padLeft, hBox)
import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import Data.Proxy
import Control.Lens (view)
import Types
import UI.Views (focusedViewWidget)
import UI.Actions (HasName(..), HasEditor(..))
import Config.Main
  (editorLabelAttr, editorAttr, editorFocusedAttr, statusbarAttr, editorErrorAttr)
import UI.Notifications (hasError)
import qualified Brick.Haskeline as HB

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
renderEditorWithLabel ::
     (HasName n, HasEditor n) => Proxy n -> T.Text -> AppState -> Widget Name
renderEditorWithLabel p label s =
  let hasFocus = name p == focusedViewWidget s
      inputW = E.renderEditor (editorDrawContent (hasError s)) hasFocus (view (editorL p) s)
      labelW = withAttr editorLabelAttr $ padRight (Pad 1) $ txt label
      eAttr =
        if hasFocus
          then editorFocusedAttr
          else editorAttr
   in labelW <+> withAttr eAttr (vLimit 1 inputW)

renderHaskeline :: T.Text -> AppState -> Widget Name
renderHaskeline label s = txt label <+> HB.render (view (asThreadsView . miSearchThreadsEditor) s)
