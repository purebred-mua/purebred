module UI.FileBrowser.Main where

import Brick.Types (Padding(..), Widget)
import Brick.Widgets.Core (str, vBox, withAttr, (<+>), padLeft)

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

drawListItem :: Bool -> (Bool, FileSystemEntry) -> Widget Name
drawListItem sel (toggled, x) = let toggled2Widget = if sel || toggled then withAttr listSelectedAttr else id
                                in toggled2Widget $ padLeft (Pad 1) $ renderFileSystemEntry x <+> fillLine

renderFileSystemEntry :: FileSystemEntry -> Widget Name
renderFileSystemEntry (Directory name) = str "📂" <+> padLeft (Pad 1) (str name)
renderFileSystemEntry (File name) = str "-" <+> padLeft (Pad 1) (str name)
