{-# LANGUAGE DataKinds #-}
module UI.GatherHeaders.Keybindings where

import qualified Graphics.Vty as V
import Types
import UI.Actions

gatherFromKeybindings :: [Keybinding 'Threads 'ComposeFrom]
gatherFromKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (noop `chain'` (focus :: Action 'Threads 'ComposeTo AppState) `chain` continue)
    ]

gatherToKeybindings :: [Keybinding 'Threads 'ComposeTo]
gatherToKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (noop `chain'` (focus :: Action 'Threads 'ComposeSubject AppState) `chain` continue)
    ]

gatherSubjectKeybindings :: [Keybinding 'Threads 'ComposeSubject]
gatherSubjectKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (noop `chain'` (focus :: Action 'ComposeView 'ComposeListOfAttachments AppState) `chain` invokeEditor)
    ]
