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

module UI.FileBrowser.Keybindings where

import qualified Graphics.Vty as V
import UI.Actions
import Types

-- | Default Keybindings
fileBrowserKeybindings :: [Keybinding 'FileBrowser 'ListOfFiles]
fileBrowserKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (noop `chain'` focus @'ComposeView @'ComposeListOfAttachments `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'q') []) (noop `chain'` focus @'ComposeView @'ComposeListOfAttachments `chain` continue)
    , Keybinding (V.EvKey V.KDown []) (listDown `chain` continue)
    , Keybinding (V.EvKey V.KUp []) (listUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'j') []) (listDown `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'k') []) (listUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar ' ') []) (toggleListItem `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'u') [V.MCtrl]) (parentDirectory `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (enterDirectory `chain` createAttachments `chain` continue)
    , Keybinding (V.EvKey (V.KChar ':') []) (noop `chain'` focus @'FileBrowser @'ManageFileBrowserSearchPath `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'G') []) (listJumpToEnd `chain` continue)
    , Keybinding (V.EvKey (V.KChar '1') []) (listJumpToStart `chain` continue)
    ]

manageSearchPathKeybindings :: [Keybinding 'FileBrowser 'ManageFileBrowserSearchPath]
manageSearchPathKeybindings =
  [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` focus @'FileBrowser @'ListOfFiles `chain` continue)
  , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` focus @'FileBrowser @'ListOfFiles `chain` continue)
  , Keybinding (V.EvKey V.KEnter []) (done `chain'` focus @'FileBrowser @'ListOfFiles `chain` continue)
  ]
