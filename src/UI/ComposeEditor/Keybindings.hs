{-# LANGUAGE DataKinds #-}

module UI.ComposeEditor.Keybindings where

import qualified Brick.Types as Brick
import qualified Graphics.Vty as V
import Prelude hiding (readFile, unlines)
import UI.Actions
import Types


composeEditorKeybindings :: [Keybinding 'ComposeView 'ComposeSubject (Brick.Next AppState)]
composeEditorKeybindings =
    [ Keybinding (V.EvKey (V.KChar '\t') []) (noop `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'y') []) (done `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEsc []) (abort `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'j') []) (listDown `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'k') []) (listUp `chain` continue)
    ]
