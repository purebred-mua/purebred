{-# LANGUAGE DataKinds #-}
module UI.GatherHeaders.Keybindings where

import qualified Brick.Types as T
import qualified Graphics.Vty as V
import Types
import UI.Actions

gatherFromKeybindings :: [Keybinding 'Threads 'ComposeFrom (T.Next AppState)]
gatherFromKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (noop `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (noop `chain'` (focus :: Action 'Threads 'ComposeTo AppState) `chain` continue)
    ]

gatherToKeybindings :: [Keybinding 'Threads 'ComposeTo (T.Next AppState)]
gatherToKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (noop `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (noop `chain'` (focus :: Action 'Threads 'ComposeSubject AppState) `chain` continue)
    ]

gatherSubjectKeybindings :: [Keybinding 'Threads 'ComposeSubject (T.Next AppState)]
gatherSubjectKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (noop `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (noop `chain'` (focus :: Action 'ComposeView 'ComposeFrom AppState) `chain` invokeEditor)
    ]
