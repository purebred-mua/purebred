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

module Purebred.UI.ComposeEditor.Keybindings where

import qualified Brick.Types as T
import qualified Graphics.Vty as V
import Purebred.UI.Actions
import Types


composeSubjectKeybindings :: [Keybinding 'ComposeView 'ComposeSubject]
composeSubjectKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `focus` continue @'ComposeView @'ComposeListOfAttachments)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `focus` continue @'ComposeView @'ComposeListOfAttachments)
    , Keybinding (V.EvKey V.KEnter []) (done `focus` continue @'ComposeView @'ComposeListOfAttachments)
    ]

composeFromKeybindings :: [Keybinding 'ComposeView 'ComposeFrom]
composeFromKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `focus` continue @'ComposeView @'ComposeListOfAttachments)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `focus` continue @'ComposeView @'ComposeListOfAttachments)
    , Keybinding (V.EvKey V.KEnter []) (done `focus` continue @'ComposeView @'ComposeListOfAttachments)
    ]

composeToKeybindings :: [Keybinding 'ComposeView 'ComposeTo]
composeToKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `focus` continue @'ComposeView @'ComposeListOfAttachments)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `focus` continue @'ComposeView @'ComposeListOfAttachments)
    , Keybinding (V.EvKey V.KEnter []) (done `focus` continue @'ComposeView @'ComposeListOfAttachments)
    ]

composeCcKeybindings :: [Keybinding 'ComposeView 'ComposeCc]
composeCcKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `focus` continue @'ComposeView @'ComposeListOfAttachments)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `focus` continue @'ComposeView @'ComposeListOfAttachments)
    , Keybinding (V.EvKey V.KEnter []) (done `focus` continue @'ComposeView @'ComposeListOfAttachments)
    ]

composeBccKeybindings :: [Keybinding 'ComposeView 'ComposeBcc]
composeBccKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `focus` continue @'ComposeView @'ComposeListOfAttachments)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `focus` continue @'ComposeView @'ComposeListOfAttachments)
    , Keybinding (V.EvKey V.KEnter []) (done `focus` continue @'ComposeView @'ComposeListOfAttachments)
    ]

confirmKeybindings :: [Keybinding 'ComposeView 'ConfirmDialog]
confirmKeybindings =
  [ Keybinding
      (V.EvKey V.KEnter [])
      (handleConfirm `focus` reloadList `chain` continue)
  , Keybinding
      (V.EvKey (V.KChar 'q') [])
      (noop `focus` continue @'ComposeView @'ComposeListOfAttachments)
  , Keybinding
      (V.EvKey V.KEsc [])
      (noop `focus` continue @'ComposeView @'ComposeListOfAttachments)
  ]

confirmAbort :: Action 'ComposeView 'ComposeListOfAttachments (T.Next AppState)
confirmAbort =
  noop `focus` continue @'ComposeView @'ConfirmDialog

listOfAttachmentsKeybindings :: [Keybinding 'ComposeView 'ComposeListOfAttachments]
listOfAttachmentsKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) confirmAbort
    , Keybinding (V.EvKey (V.KChar 'q') []) confirmAbort
    , Keybinding (V.EvKey V.KDown []) (listDown `chain` continue)
    , Keybinding (V.EvKey V.KUp []) (listUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'j') []) (listDown `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'k') []) (listUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'G') []) (listJumpToEnd `chain` continue)
    , Keybinding (V.EvKey (V.KChar '1') []) (listJumpToStart `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'y') [])
        (ifte done (switchView @'Threads @'ListOfThreads) noop *> continue)
    , Keybinding (V.EvKey (V.KChar '\t') []) (noop `focus` continue @'Threads @'ListOfThreads)
    , Keybinding (V.EvKey (V.KChar 'e') []) edit
    , Keybinding (V.EvKey (V.KChar 'D') []) (delete `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'a') []) (noop `focus` continue @'FileBrowser @'ListOfFiles)
    , Keybinding (V.EvKey (V.KChar 't') []) (noop `focus` continue @'ComposeView @'ComposeTo)
    , Keybinding (V.EvKey (V.KChar 'c') []) (noop `focus` continue @'ComposeView @'ComposeCc)
    , Keybinding (V.EvKey (V.KChar 'b') []) (noop `focus` continue @'ComposeView @'ComposeBcc)
    , Keybinding (V.EvKey (V.KChar 's') []) (noop `focus` continue @'ComposeView @'ComposeSubject)
    , Keybinding (V.EvKey (V.KChar 'f') []) (noop `focus` continue @'ComposeView @'ComposeFrom)
    ]
