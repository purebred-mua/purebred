{-# LANGUAGE OverloadedStrings #-}

module UI.ComposeEditor.Keybindings where

import qualified Brick.Widgets.Edit as E
import Data.Text (Text)
import qualified Graphics.Vty as V
import Prelude hiding (readFile, unlines)
import UI.Actions
import Types


composeEditorKeybindings :: [Keybinding (E.Editor Text Name)]
composeEditorKeybindings =
    [ Keybinding (V.EvKey (V.KChar '\t') []) backToIndex
    , Keybinding (V.EvKey (V.KChar 'y') []) send
    , Keybinding (V.EvKey V.KEsc []) reset
    ]
