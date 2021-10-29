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

module Purebred.UI.GatherHeaders.Keybindings where

import qualified Graphics.Vty as V
import Purebred.Types
import Purebred.UI.Actions
import qualified Brick.Types as T

gatherFromKeybindings :: [Keybinding 'Threads 'ComposeFrom]
gatherFromKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `focus` continue @'Threads @'ListOfThreads)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `focus` continue @'Threads @'ListOfThreads)
    , Keybinding (V.EvKey V.KEnter []) (noop `focus` continue @'Threads @'ComposeTo)
    ]

gatherToKeybindings :: [Keybinding 'Threads 'ComposeTo]
gatherToKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `focus` continue @'Threads @'ListOfThreads)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `focus` continue @'Threads @'ListOfThreads)
    , Keybinding (V.EvKey V.KEnter []) (noop `focus` continue @'Threads @'ComposeSubject)
    ]

gatherSubjectKeybindings :: [Keybinding 'Threads 'ComposeSubject]
gatherSubjectKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `focus` continue @'Threads @'ListOfThreads)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `focus` continue @'Threads @'ListOfThreads)
    , Keybinding (V.EvKey V.KEnter []) (
        noop
        `focus` (
            invokeEditor Threads ListOfThreads
            :: Action 'ComposeView 'ComposeListOfAttachments (T.Next AppState))
        )
    ]
