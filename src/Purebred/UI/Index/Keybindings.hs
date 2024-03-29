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

module Purebred.UI.Index.Keybindings
  ( browseThreadsKeybindings
  , searchThreadsKeybindings
  , manageThreadTagsKeybindings
  ) where

import qualified Graphics.Vty as V
import Purebred.UI.Actions
import Purebred.Types

browseThreadsKeybindings :: [Keybinding 'Threads 'ListOfThreads]
browseThreadsKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) quit
    , Keybinding (V.EvKey (V.KChar 'q') []) quit
    , Keybinding (V.EvKey V.KEnter []) (displayThreadMails !*> selectNextUnread !*> displayMail)
    , Keybinding (V.EvKey (V.KChar ':') []) (switchView @'Threads @'SearchThreadsEditor)
    , Keybinding (V.EvKey (V.KChar 'm') []) (switchView @'Threads @'ComposeFrom)
    , Keybinding (V.EvKey (V.KChar '`') []) (switchView @'Threads @'ManageThreadTagsEditor)
    , Keybinding (V.EvKey (V.KChar '\t') []) switchComposeEditor
    , Keybinding (V.EvKey (V.KChar '?') []) (switchView @'Help @'ScrollingHelpView)
    , Keybinding (V.EvKey (V.KChar 'j') []) listDown
    , Keybinding (V.EvKey (V.KChar 'k') []) listUp
    , Keybinding (V.EvKey V.KDown []) listDown
    , Keybinding (V.EvKey V.KUp []) listUp
    , Keybinding (V.EvKey (V.KChar 'G') []) listJumpToEnd
    , Keybinding (V.EvKey (V.KChar '1') []) listJumpToStart
    , Keybinding (V.EvKey (V.KChar '*') []) (toggleListItem *> listDown)
    , Keybinding (V.EvKey (V.KChar '+') []) searchRelated
    ]

searchThreadsKeybindings :: [Keybinding 'Threads 'SearchThreadsEditor]
searchThreadsKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort *> switchView @'Threads @'ListOfThreads)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort *> switchView @'Threads @'ListOfThreads)
    , Keybinding (V.EvKey V.KEnter []) (done *> switchView @'Threads @'ListOfThreads)
    ]

manageThreadTagsKeybindings :: [Keybinding 'Threads 'ManageThreadTagsEditor]
manageThreadTagsKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort *> switchView @'Threads @'ListOfThreads)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort *> switchView @'Threads @'ListOfThreads)
    , Keybinding (V.EvKey V.KEnter []) (done !*> untoggleListItems @'Threads @'ListOfThreads)
    ]
