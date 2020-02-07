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

module UI.Help.Keybindings where

import Graphics.Vty (Event (..), Key (..))
import UI.Actions
import Types

-- | Default Keybindings
helpKeybindings :: [Keybinding 'Help 'ScrollingHelpView]
helpKeybindings =
    [ Keybinding (EvKey KEsc []) (noop `chain'` focus @'Threads @'ListOfThreads `chain` continue)
    , Keybinding (EvKey (KChar 'q') []) (noop `chain'` focus @'Threads @'ListOfThreads `chain` continue)
    , Keybinding (EvKey KBS []) (scrollPageUp `chain` continue)
    , Keybinding (EvKey (KChar ' ') []) (scrollPageDown `chain` continue)
    , Keybinding (EvKey KBS []) (scrollPageUp `chain` continue)
    ]
