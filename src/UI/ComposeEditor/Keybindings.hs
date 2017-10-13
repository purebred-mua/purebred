{-# LANGUAGE OverloadedStrings #-}

module UI.ComposeEditor.Keybindings where

import qualified Brick.Widgets.Edit as E
import qualified Brick.Types as T
import Data.Text (Text)
import qualified Graphics.Vty as V
import Prelude hiding (readFile, unlines)
import UI.Actions
import Types


composeEditorKeybindings :: [Keybinding (E.Editor Text Name) (T.Next AppState)]
composeEditorKeybindings =
    [ Keybinding (V.EvKey (V.KChar '\t') []) (backToIndex `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'y') []) (send `chain` continue)
    , Keybinding (V.EvKey V.KEsc []) (reset `chain` continue)
    ]
