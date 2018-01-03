{-# LANGUAGE DataKinds #-}

module UI.ComposeEditor.Keybindings where

import qualified Brick.Types as Brick
import qualified Graphics.Vty as V
import Prelude hiding (readFile, unlines)
import UI.Actions
import Types


composeEditorKeybindings :: [Keybinding 'ComposeEditor (Brick.Next AppState)]
composeEditorKeybindings =
    [ Keybinding (V.EvKey (V.KChar 'n') [V.MCtrl]) (nextInput `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'p') [V.MCtrl]) (previousInput `chain` continue)
    , Keybinding (V.EvKey (V.KChar '\t') []) (noop `chain'` (focus :: Action 'BrowseThreads AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'y') []) (done `chain'` (focus :: Action 'BrowseThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEsc []) (abort `chain'` (focus :: Action 'BrowseThreads AppState) `chain` continue)
    ]
