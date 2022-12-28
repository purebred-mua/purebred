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

module Purebred.UI.ComposeEditor.Keybindings
  ( composeSubjectKeybindings
  , composeFromKeybindings
  , composeToKeybindings
  , composeCcKeybindings
  , composeBccKeybindings
  , confirmKeybindings
  , listOfAttachmentsKeybindings
  ) where

import qualified Graphics.Vty as V
import Purebred.UI.Actions
import Purebred.Types


composeSubjectKeybindings :: [Keybinding 'ComposeView 'ComposeSubject]
composeSubjectKeybindings =
    [ Keybinding (V.EvKey V.KEsc [])
        (abort *> switchView @'ComposeView @'ComposeListOfAttachments)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl])
        (abort *> switchView @'ComposeView @'ComposeListOfAttachments)
    , Keybinding (V.EvKey V.KEnter [])
        (done *> switchView @'ComposeView @'ComposeListOfAttachments)
    ]

composeFromKeybindings :: [Keybinding 'ComposeView 'ComposeFrom]
composeFromKeybindings =
    [ Keybinding (V.EvKey V.KEsc [])
        (abort *> switchView @'ComposeView @'ComposeListOfAttachments)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl])
        (abort *> switchView @'ComposeView @'ComposeListOfAttachments)
    , Keybinding (V.EvKey V.KEnter [])
        (done *> switchView @'ComposeView @'ComposeListOfAttachments)
    ]

composeToKeybindings :: [Keybinding 'ComposeView 'ComposeTo]
composeToKeybindings =
    [ Keybinding (V.EvKey V.KEsc [])
        (abort *> switchView @'ComposeView @'ComposeListOfAttachments)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl])
        (abort *> switchView @'ComposeView @'ComposeListOfAttachments)
    , Keybinding (V.EvKey V.KEnter [])
        (done *> switchView @'ComposeView @'ComposeListOfAttachments)
    ]

composeCcKeybindings :: [Keybinding 'ComposeView 'ComposeCc]
composeCcKeybindings =
    [ Keybinding (V.EvKey V.KEsc [])
        (abort *> switchView @'ComposeView @'ComposeListOfAttachments)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl])
        (abort *> switchView @'ComposeView @'ComposeListOfAttachments)
    , Keybinding (V.EvKey V.KEnter [])
        (done *> switchView @'ComposeView @'ComposeListOfAttachments)
    ]

composeBccKeybindings :: [Keybinding 'ComposeView 'ComposeBcc]
composeBccKeybindings =
    [ Keybinding (V.EvKey V.KEsc [])
        (abort *> switchView @'ComposeView @'ComposeListOfAttachments)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl])
        (abort *> switchView @'ComposeView @'ComposeListOfAttachments)
    , Keybinding (V.EvKey V.KEnter [])
        (done *> switchView @'ComposeView @'ComposeListOfAttachments)
    ]

confirmKeybindings :: [Keybinding 'ComposeView 'ConfirmDialog]
confirmKeybindings =
  [ Keybinding
      (V.EvKey V.KEnter [])
      (handleConfirm !*> reloadList)
  , Keybinding
      (V.EvKey (V.KChar 'q') [])
      (abort *> switchView @'ComposeView @'ComposeListOfAttachments)
  , Keybinding
      (V.EvKey V.KEsc [])
      (abort *> switchView @'ComposeView @'ComposeListOfAttachments)
  ]

confirmAbort :: Action 'ComposeView 'ComposeListOfAttachments ()
confirmAbort = switchView @'ComposeView @'ConfirmDialog

listOfAttachmentsKeybindings :: [Keybinding 'ComposeView 'ComposeListOfAttachments]
listOfAttachmentsKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) confirmAbort
    , Keybinding (V.EvKey (V.KChar 'q') []) confirmAbort
    , Keybinding (V.EvKey V.KDown []) listDown
    , Keybinding (V.EvKey V.KUp []) listUp
    , Keybinding (V.EvKey (V.KChar 'j') []) listDown
    , Keybinding (V.EvKey (V.KChar 'k') []) listUp
    , Keybinding (V.EvKey (V.KChar 'G') []) listJumpToEnd
    , Keybinding (V.EvKey (V.KChar '1') []) listJumpToStart
    , Keybinding (V.EvKey (V.KChar 'y') [])
        (ifte done (switchView @'Threads @'ListOfThreads) noop)
    , Keybinding (V.EvKey (V.KChar '\t') []) (switchView @'Threads @'ListOfThreads)
    , Keybinding (V.EvKey (V.KChar 'e') []) edit
    , Keybinding (V.EvKey (V.KChar 'D') []) delete
    , Keybinding (V.EvKey (V.KChar 'a') []) (switchView @'FileBrowser @'ListOfFiles)
    , Keybinding (V.EvKey (V.KChar 't') []) (switchView @'ComposeView @'ComposeTo)
    , Keybinding (V.EvKey (V.KChar 'c') []) (switchView @'ComposeView @'ComposeCc)
    , Keybinding (V.EvKey (V.KChar 'b') []) (switchView @'ComposeView @'ComposeBcc)
    , Keybinding (V.EvKey (V.KChar 's') []) (switchView @'ComposeView @'ComposeSubject)
    , Keybinding (V.EvKey (V.KChar 'f') []) (switchView @'ComposeView @'ComposeFrom)
    ]
