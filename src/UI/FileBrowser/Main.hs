-- This file is part of purebred
-- Copyright (C) 2018 Róman Joost
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
module UI.FileBrowser.Main
       (renderFileBrowser, renderFileBrowserSearchPathEditor) where

import Brick.Types (Widget)
import Brick.Widgets.Core (hBox, str, txt, withAttr, (<+>), vLimit)
import qualified Brick.Widgets.Edit as E

import qualified Brick.Widgets.List as L
import Control.Lens.Getter (view)
import Config.Main (listSelectedAttr, listAttr)
import UI.Draw.Main (fillLine)
import UI.Views (focusedViewWidget)
import Types

renderFileBrowser :: AppState -> Widget Name
renderFileBrowser s = L.renderList drawListItem (ListOfFiles == focusedViewWidget s)
                      $ view (asFileBrowser . fbEntries) s

drawListItem :: Bool -> Toggleable FileSystemEntry -> Widget Name
drawListItem sel (toggled, x) =
  let attr = withAttr $ if sel then listSelectedAttr else listAttr
      toggledWidget = txt $ if toggled then " ☑ " else " ☐ "
  in
    attr $ hBox
      [ toggledWidget
      , renderFileSystemEntry x
      , fillLine
      ]

renderFileBrowserSearchPathEditor :: AppState -> Widget Name
renderFileBrowserSearchPathEditor s =
  let hasFocus = ManageFileBrowserSearchPath == focusedViewWidget s
      editorDrawContent = str . unlines
      inputW = E.renderEditor editorDrawContent hasFocus (view (asFileBrowser . fbSearchPath) s)
      labelW = txt "Path: "
  in labelW <+> vLimit 1 inputW

renderFileSystemEntry :: FileSystemEntry -> Widget Name
renderFileSystemEntry (Directory name) = txt "📂 " <+> str name
renderFileSystemEntry (File name) = txt "- " <+> str name
