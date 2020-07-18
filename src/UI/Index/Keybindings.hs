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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module UI.Index.Keybindings where

import qualified Graphics.Vty as V
import UI.Actions
import Types

browseThreadsKeybindings :: [Keybinding 'Threads 'ListOfThreads]
browseThreadsKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) quit
    , Keybinding (V.EvKey (V.KChar 'q') []) quit
    , Keybinding (V.EvKey V.KEnter []) (displayThreadMails `focus` selectNextUnread `focus` displayMail `chain` continue)
    , Keybinding (V.EvKey (V.KChar ':') []) (noop `focus` continue @'Threads @'SearchThreadsEditor)
    , Keybinding (V.EvKey (V.KChar 'm') []) (noop `focus` continue @'Threads @'ComposeFrom)
    , Keybinding (V.EvKey (V.KChar '`') []) (noop `focus` continue @'Threads @'ManageThreadTagsEditor)
    , Keybinding (V.EvKey (V.KChar '\t') []) (switchComposeEditor `chain` continue)
    , Keybinding (V.EvKey (V.KChar '?') []) (noop `focus` continue @'Help @'ScrollingHelpView)
    , Keybinding (V.EvKey (V.KChar 'j') []) (listDown `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'k') []) (listUp `chain` continue)
    , Keybinding (V.EvKey V.KDown []) (listDown `chain` continue)
    , Keybinding (V.EvKey V.KUp []) (listUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'G') []) (listJumpToEnd `chain` continue)
    , Keybinding (V.EvKey (V.KChar '1') []) (listJumpToStart `chain` continue)
    , Keybinding (V.EvKey (V.KChar '*') []) (toggleListItem `chain` listDown `chain` continue)
    ]

searchThreadsKeybindings :: [Keybinding 'Threads 'SearchThreadsEditor]
searchThreadsKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `focus` continue @'Threads @'ListOfThreads)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `focus` continue @'Threads @'ListOfThreads)
    , Keybinding (V.EvKey V.KEnter []) (done `focus` continue @'Threads @'ListOfThreads)
    ]

manageThreadTagsKeybindings :: [Keybinding 'Threads 'ManageThreadTagsEditor]
manageThreadTagsKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `focus` continue @'Threads @'ListOfThreads)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `focus` continue @'Threads @'ListOfThreads)
    , Keybinding (V.EvKey V.KEnter []) (done `focus` untoggleListItems @'Threads @'ListOfThreads `chain` continue)
    ]
