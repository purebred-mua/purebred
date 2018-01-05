{-# LANGUAGE DataKinds #-}
module UI.FileBrowser.Keybindings where

import qualified Graphics.Vty as V
import UI.Actions
import Types

-- | Default Keybindings
fileBrowserKeybindings :: [Keybinding 'FileBrowser 'ListOfFiles]
fileBrowserKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (noop `chain'` (focus :: Action 'ComposeView 'ListOfAttachments AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'q') []) (noop `chain'` (focus :: Action 'ComposeView 'ListOfAttachments AppState) `chain` continue)
    , Keybinding (V.EvKey V.KDown []) (listDown `chain` continue)
    , Keybinding (V.EvKey V.KUp []) (listUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'j') []) (listDown `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'k') []) (listUp `chain` continue)
    -- Enter
    ]

manageSearchPathKeybindings :: [Keybinding 'FileBrowser 'ManageFileBrowserSearchPath]
manageSearchPathKeybindings = []
