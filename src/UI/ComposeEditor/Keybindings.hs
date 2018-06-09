{-# LANGUAGE DataKinds #-}

module UI.ComposeEditor.Keybindings where

import Data.Semigroup ((<>))
import qualified Brick.Types as Brick
import qualified Graphics.Vty as V
import Prelude hiding (readFile, unlines)
import UI.Actions
import Types


commonKeybindings :: [Keybinding 'ComposeView ctx (Brick.Next AppState)]
commonKeybindings =
    [ Keybinding (V.EvKey (V.KChar 'n') [V.MCtrl]) (focusNextWidget `chain` continue)
    ]

composeSubjectKeybindings :: [Keybinding 'ComposeView 'ComposeSubject (Brick.Next AppState)]
composeSubjectKeybindings =
    [ Keybinding (V.EvKey (V.KChar '\t') []) (noop `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEsc []) (noop `chain'` (focus :: Action 'ComposeView 'ListOfAttachments AppState) `chain` continue)
    ] <> commonKeybindings

composeFromKeybindings :: [Keybinding 'ComposeView 'ComposeFrom (Brick.Next AppState)]
composeFromKeybindings =
    [ Keybinding (V.EvKey (V.KChar '\t') []) (noop `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEsc []) (abort `chain` continue)
    ] <> commonKeybindings

composeToKeybindings :: [Keybinding 'ComposeView 'ComposeTo (Brick.Next AppState)]
composeToKeybindings =
    [ Keybinding (V.EvKey (V.KChar '\t') []) (noop `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEsc []) (abort `chain` continue)
    ] <> commonKeybindings

listOfAttachmentsKeybindings :: [Keybinding 'ComposeView 'ListOfAttachments (Brick.Next AppState)]
listOfAttachmentsKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'q') []) (abort `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KDown []) (listDown `chain` continue)
    , Keybinding (V.EvKey V.KUp []) (listUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'j') []) (listDown `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'k') []) (listUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'G') []) (listJumpToEnd `chain` continue)
    , Keybinding (V.EvKey (V.KChar '1') []) (listJumpToStart `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'y') []) (done `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar '\t') []) (noop `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    ] <> commonKeybindings
