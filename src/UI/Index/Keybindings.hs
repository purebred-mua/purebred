{-# LANGUAGE DataKinds #-}

module UI.Index.Keybindings where

import qualified Graphics.Vty as V
import UI.Actions
import Types

-- | Default Keybindings
browseMailKeybindings :: [Keybinding 'Mails 'ListOfMails]
browseMailKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (noop `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'q') []) (noop `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (noop `chain'` displayMail `chain` continue)
    , Keybinding (V.EvKey V.KDown []) (listDown `chain` continue)
    , Keybinding (V.EvKey V.KUp []) (listUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'j') []) (listDown `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'k') []) (listUp `chain` continue)
    , Keybinding (V.EvKey V.KDown []) (listDown `chain` continue)
    , Keybinding (V.EvKey V.KUp []) (listUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'G') []) (listJumpToEnd `chain` continue)
    , Keybinding (V.EvKey (V.KChar '1') []) (listJumpToStart `chain` continue)
    , Keybinding (V.EvKey (V.KChar 't') []) (setUnread `chain` continue)
    , Keybinding (V.EvKey (V.KChar '?') []) (noop `chain'` (focus :: Action 'Help 'ScrollingHelpView AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar '`') []) (noop `chain'` (focus :: Action 'Mails 'ManageMailTagsEditor AppState) `chain` continue)
    ]

browseThreadsKeybindings :: [Keybinding 'Threads 'ListOfThreads]
browseThreadsKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) quit
    , Keybinding (V.EvKey (V.KChar 'q') []) quit
    , Keybinding (V.EvKey V.KEnter []) (displayThreadMails `chain'` (focus :: Action 'Mails 'ListOfMails AppState) `chain'` selectNextUnread `chain'` displayMail `chain` continue)
    , Keybinding (V.EvKey (V.KChar ':') []) (noop `chain'` (focus :: Action 'Threads 'SearchThreadsEditor AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'm') []) (noop `chain'` (focus :: Action 'Threads 'ComposeFrom AppState) `chain`continue)
    , Keybinding (V.EvKey (V.KChar '`') []) (noop `chain'` (focus :: Action 'Threads 'ManageThreadTagsEditor AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar '\t') []) (switchComposeEditor `chain` continue)
    , Keybinding (V.EvKey (V.KChar '?') []) (noop `chain'` (focus :: Action 'Help 'ScrollingHelpView AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'j') []) (listDown `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'k') []) (listUp `chain` continue)
    , Keybinding (V.EvKey V.KDown []) (listDown `chain` continue)
    , Keybinding (V.EvKey V.KUp []) (listUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'G') []) (listJumpToEnd `chain` continue)
    , Keybinding (V.EvKey (V.KChar '1') []) (listJumpToStart `chain` continue)
    ]

searchThreadsKeybindings :: [Keybinding 'Threads 'SearchThreadsEditor]
searchThreadsKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (done `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    ]

manageMailTagsKeybindings :: [Keybinding 'Mails 'ManageMailTagsEditor]
manageMailTagsKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` (focus :: Action 'Mails 'ListOfMails AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` (focus :: Action 'Mails 'ListOfMails AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (done `chain'` (focus :: Action 'Mails 'ListOfMails AppState) `chain` continue)
    ]

manageThreadTagsKeybindings :: [Keybinding 'Threads 'ManageThreadTagsEditor]
manageThreadTagsKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'g') [V.MCtrl]) (abort `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (done `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    ]
