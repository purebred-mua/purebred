{-# LANGUAGE OverloadedStrings #-}
module UI.FileBrowser.Main
       (renderFileBrowser, renderFileBrowserSearchPathEditor) where

import Brick.Types (Widget, Padding(..))
import Brick.Widgets.Core
       (str, txt, withAttr, (<+>), vLimit, padRight, padLeft)
import qualified Brick.Widgets.Edit as E

import qualified Brick.Widgets.List as L
import Control.Lens.Getter (view)
import Config.Main (listSelectedAttr, listAttr)
import UI.Draw.Main (fillLine)
import UI.Utils (focusedViewWidget)
import Types

renderFileBrowser :: AppState -> Widget Name
renderFileBrowser s = L.renderList drawListItem (ListOfFiles == focusedViewWidget s ListOfThreads)
                      $ view (asFileBrowser . fbEntries) s

drawListItem :: Bool -> (Bool, FileSystemEntry) -> Widget Name
drawListItem sel (toggled, x) = let attr = if sel then withAttr listSelectedAttr else withAttr listAttr
                                    toggledWidget = if toggled then txt "☑" else txt "☐"
                                in attr $ padLeft (Pad 1) $ toggledWidget
                                   <+> padLeft (Pad 1) (renderFileSystemEntry x)
                                   <+> fillLine

renderFileBrowserSearchPathEditor :: AppState -> Widget Name
renderFileBrowserSearchPathEditor s =
  let hasFocus = ManageFileBrowserSearchPath == focusedViewWidget s ListOfThreads
      editorDrawContent = str . unlines
      inputW = E.renderEditor editorDrawContent hasFocus (view (asFileBrowser . fbSearchPath) s)
      labelW = padRight (Pad 1) $ txt "Path:"
  in labelW <+> vLimit 1 inputW

renderFileSystemEntry :: FileSystemEntry -> Widget Name
renderFileSystemEntry (Directory name) = txt "📂" <+> padLeft (Pad 1) (str name)
renderFileSystemEntry (File name) = txt "-" <+> padLeft (Pad 1) (str name)
