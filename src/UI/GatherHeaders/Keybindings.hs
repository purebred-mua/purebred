{-# LANGUAGE DataKinds #-}
module UI.GatherHeaders.Keybindings where

import qualified Brick.Types as T
import qualified Graphics.Vty as V
import Types
import UI.Actions

interactiveGatherHeadersKeybindings :: [Keybinding 'GatherHeadersFrom (T.Next AppState)]
interactiveGatherHeadersKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (noop `chain'` (focus :: Action 'BrowseThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (noop `chain'` (focus :: Action 'GatherHeadersTo AppState) `chain` continue)
    ]

interactiveGatherHeadersToKeybindings :: [Keybinding 'GatherHeadersTo (T.Next AppState)]
interactiveGatherHeadersToKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (noop `chain'` (focus :: Action 'BrowseThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (noop `chain'` (focus :: Action 'GatherHeadersSubject AppState) `chain` continue)
    ]

interactiveGatherHeadersSubjectKeybindings :: [Keybinding 'GatherHeadersSubject (T.Next AppState)]
interactiveGatherHeadersSubjectKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (noop `chain'` (focus :: Action 'BrowseThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (noop `chain'` (focus :: Action 'ComposeEditor AppState) `chain` invokeEditor)
    ]
