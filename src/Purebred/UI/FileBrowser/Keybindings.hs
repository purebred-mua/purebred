-- This file is part of purebred
-- Copyright (C) 2018-2019 Róman Joost
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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Purebred.UI.FileBrowser.Keybindings where

import qualified Graphics.Vty as V
import Purebred.UI.Actions
import Purebred.Types

-- | Default Keybindings
fileBrowserKeybindings :: [Keybinding 'FileBrowser 'ListOfFiles]
fileBrowserKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (switchView @'ComposeView @'ComposeListOfAttachments)
    , Keybinding (V.EvKey (V.KChar 'q') []) (switchView @'ComposeView @'ComposeListOfAttachments)
    , Keybinding (V.EvKey (V.KChar ':') []) (switchView @'FileBrowser @'ManageFileBrowserSearchPath)
    , Keybinding (V.EvKey V.KEnter []) createAttachments
    , Keybinding (V.EvKey (V.KChar '*') []) fileBrowserToggleFile
    ]

manageSearchPathKeybindings :: [Keybinding 'FileBrowser 'ManageFileBrowserSearchPath]
manageSearchPathKeybindings =
  [ Keybinding (V.EvKey V.KEsc []) (abort *> switchView @'FileBrowser @'ListOfFiles)
  , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort *> switchView @'FileBrowser @'ListOfFiles)
  , Keybinding (V.EvKey V.KEnter []) (done *> switchView @'FileBrowser @'ListOfFiles)
  ]
