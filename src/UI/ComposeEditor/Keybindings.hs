{-# LANGUAGE DataKinds #-}

module UI.ComposeEditor.Keybindings where

import Data.Semigroup ((<>))
import qualified Graphics.Vty as V
import UI.Actions
import Types


commonKeybindings :: [Keybinding 'ComposeView ctx]
commonKeybindings =
    [ Keybinding (V.EvKey (V.KChar 'n') [V.MCtrl]) (focusNextWidget `chain` continue)
    ]

composeSubjectKeybindings :: [Keybinding 'ComposeView 'ComposeSubject]
composeSubjectKeybindings =
    [ Keybinding (V.EvKey (V.KChar '\t') []) (noop `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEsc []) (noop `chain'` (focus :: Action 'ComposeView 'ListOfAttachments AppState) `chain` continue)
    ] <> commonKeybindings

composeFromKeybindings :: [Keybinding 'ComposeView 'ComposeFrom]
composeFromKeybindings =
    [ Keybinding (V.EvKey (V.KChar '\t') []) (noop `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEsc []) (abort `chain` continue)
    ] <> commonKeybindings

composeToKeybindings :: [Keybinding 'ComposeView 'ComposeTo]
composeToKeybindings =
    [ Keybinding (V.EvKey (V.KChar '\t') []) (noop `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEsc []) (abort `chain` continue)
    ] <> commonKeybindings

listOfAttachmentsKeybindings :: [Keybinding 'ComposeView 'ListOfAttachments]
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
    , Keybinding (V.EvKey (V.KChar 'e') []) edit
    , Keybinding (V.EvKey (V.KChar 'a') []) (noop `chain'` (focus :: Action 'FileBrowser 'ListOfFiles AppState) `chain` continue)
    ] <> commonKeybindings
