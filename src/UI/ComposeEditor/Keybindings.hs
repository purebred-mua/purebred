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

module UI.ComposeEditor.Keybindings where

import qualified Brick.Types as T
import qualified Graphics.Vty as V
import UI.Actions
import Types


composeSubjectKeybindings :: [Keybinding 'ComposeView 'ComposeSubject]
composeSubjectKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` focus @'ComposeView @'ComposeListOfAttachments `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` focus @'ComposeView @'ComposeListOfAttachments `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (done `chain'` focus @'ComposeView @'ComposeListOfAttachments `chain` continue)
    ]

composeFromKeybindings :: [Keybinding 'ComposeView 'ComposeFrom]
composeFromKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` focus @'ComposeView @'ComposeListOfAttachments `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` focus @'ComposeView @'ComposeListOfAttachments `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (done `chain'` focus @'ComposeView @'ComposeListOfAttachments `chain` continue)
    ]

composeToKeybindings :: [Keybinding 'ComposeView 'ComposeTo]
composeToKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` focus @'ComposeView @'ComposeListOfAttachments `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` focus @'ComposeView @'ComposeListOfAttachments `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (done `chain'` focus @'ComposeView @'ComposeListOfAttachments `chain` continue)
    ]

composeCcKeybindings :: [Keybinding 'ComposeView 'ComposeCc]
composeCcKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` focus @'ComposeView @'ComposeListOfAttachments `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` focus @'ComposeView @'ComposeListOfAttachments `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (done `chain'` focus @'ComposeView @'ComposeListOfAttachments `chain` continue)
    ]

composeBccKeybindings :: [Keybinding 'ComposeView 'ComposeBcc]
composeBccKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` focus @'ComposeView @'ComposeListOfAttachments `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` focus @'ComposeView @'ComposeListOfAttachments `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (done `chain'` focus @'ComposeView @'ComposeListOfAttachments `chain` continue)
    ]

confirmKeybindings :: [Keybinding 'ComposeView 'ConfirmDialog]
confirmKeybindings =
  [ Keybinding
      (V.EvKey V.KEnter [])
      (handleConfirm `chain'` focus @'Threads @'ListOfThreads `chain'`
       reloadList `chain`
       continue)
  , Keybinding
      (V.EvKey (V.KChar 'q') [])
      (noop `chain'`
       focus @'ComposeView @'ComposeListOfAttachments `chain`
       continue)
  , Keybinding
      (V.EvKey V.KEsc [])
      (noop `chain'`
       focus @'ComposeView @'ComposeListOfAttachments `chain`
       continue)
  ]

confirmAbort :: Action 'ComposeView 'ComposeListOfAttachments (T.Next AppState)
confirmAbort =
  noop `chain'` focus @'ComposeView @'ConfirmDialog `chain`
  continue

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
    , Keybinding (V.EvKey (V.KChar 'y') []) (done `chain'` focus @'Threads @'ListOfThreads `chain` continue)
    , Keybinding (V.EvKey (V.KChar '\t') []) (noop `chain'` focus @'Threads @'ListOfThreads `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'e') []) edit
    , Keybinding (V.EvKey (V.KChar 'D') []) (delete `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'a') []) (noop `chain'` focus @'FileBrowser @'ListOfFiles `chain` continue)
    , Keybinding (V.EvKey (V.KChar 't') []) (noop `chain'` focus @'ComposeView @'ComposeTo `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'c') []) (noop `chain'` focus @'ComposeView @'ComposeCc `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'b') []) (noop `chain'` focus @'ComposeView @'ComposeBcc `chain` continue)
    , Keybinding (V.EvKey (V.KChar 's') []) (noop `chain'` focus @'ComposeView @'ComposeSubject `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'f') []) (noop `chain'` focus @'ComposeView @'ComposeFrom `chain` continue)
    ]
