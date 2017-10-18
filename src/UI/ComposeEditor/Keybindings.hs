{-# LANGUAGE DataKinds #-}

module UI.ComposeEditor.Keybindings where

import qualified Brick.Types as Brick
import qualified Graphics.Vty as V
import Prelude hiding (readFile, unlines)
import UI.Actions
import Types


composeEditorKeybindings :: [Keybinding 'ComposeEditor (Brick.Next AppState)]
composeEditorKeybindings =
    [ Keybinding (V.EvKey (V.KChar '\t') []) (backToIndex `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'y') []) (done `chain` backToIndex `chain` continue)
    , Keybinding (V.EvKey V.KEsc []) (abort `chain` backToIndex `chain` continue)
    ]
