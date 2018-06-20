{-# LANGUAGE DataKinds #-}

module UI.Help.Keybindings where

import Graphics.Vty (Event (..), Key (..))
import UI.Actions
import Types

-- | Default Keybindings
helpKeybindings :: [Keybinding 'Help 'ScrollingHelpView]
helpKeybindings =
    [ Keybinding (EvKey KEsc []) (noop `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (EvKey KBS []) (scrollPageUp `chain` continue)
    , Keybinding (EvKey (KChar ' ') []) (scrollPageDown `chain` continue)
    ]
