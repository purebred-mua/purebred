module UI.FileBrowser.Main where

import Brick.Types (Widget)
import Brick.Widgets.Core (str, vBox, withAttr, (<+>))

import qualified Brick.Widgets.List as L
import Control.Lens.Getter (view)
import UI.Status.Main (statusbar)
import Config.Main (listSelectedAttr)
import UI.Draw.Main (fillLine)
import Types

drawFileBrowser :: AppState -> [Widget Name]
drawFileBrowser s = [ui]
  where
    ui = vBox [renderList s, statusbar s]

renderList :: AppState -> Widget Name
renderList s = L.renderList drawListItem True $ view (asBrowseFiles . bfEntries) s

drawListItem :: Bool -> FileSystemEntry -> Widget Name
drawListItem sel x = let toggled2Widget = if sel then withAttr listSelectedAttr else id
                     in toggled2Widget $ str (show x) <+> fillLine
