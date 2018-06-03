{-# LANGUAGE DataKinds #-}
module UI.GatherHeaders.Keybindings where

import qualified Brick.Types as T
import qualified Graphics.Vty as V
import Types
import UI.Actions

interactiveGatherHeadersKeybindings :: [Keybinding 'Threads 'ComposeFrom (T.Next AppState)]
interactiveGatherHeadersKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (noop `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (noop `chain'` (focus :: Action 'Threads 'ComposeTo AppState) `chain` continue)
    ]

composeFromKeybindings :: [Keybinding 'ComposeView 'ComposeFrom (T.Next AppState)]
composeFromKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (noop `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (noop `chain'` (focus :: Action 'Threads 'ComposeTo AppState) `chain` continue)
    ]

interactiveGatherHeadersToKeybindings :: [Keybinding 'Threads 'ComposeTo (T.Next AppState)]
interactiveGatherHeadersToKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (noop `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (noop `chain'` (focus :: Action 'Threads 'ComposeSubject AppState) `chain` continue)
    ]

composeToKeybindings :: [Keybinding 'ComposeView 'ComposeTo (T.Next AppState)]
composeToKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (noop `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (noop `chain'` (focus :: Action 'Threads 'ComposeSubject AppState) `chain` continue)
    ]

interactiveGatherHeadersSubjectKeybindings :: [Keybinding 'Threads 'ComposeSubject (T.Next AppState)]
interactiveGatherHeadersSubjectKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (noop `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (noop `chain'` (focus :: Action 'ComposeView 'ComposeFrom AppState) `chain` invokeEditor)
    ]

composeSubjectKeybindings :: [Keybinding 'ComposeView 'ComposeSubject (T.Next AppState)]
composeSubjectKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (noop `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (noop `chain'` (focus :: Action 'ComposeView 'ComposeFrom AppState) `chain` invokeEditor)
    ]
