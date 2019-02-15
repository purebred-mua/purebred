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

drawListItem :: Bool -> (Bool, FileSystemEntry) -> Widget Name
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
