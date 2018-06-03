{-# LANGUAGE DataKinds #-}

module UI.Help.Keybindings where

import qualified Brick.Types as Brick
import Graphics.Vty (Event (..), Key (..))
import UI.Actions
import Types

-- | Default Keybindings
helpKeybindings :: [Keybinding 'Help 'ScrollingHelpView (Brick.Next AppState)]
helpKeybindings =
    [ Keybinding (EvKey KEsc []) (noop `chain'` (focus :: Action 'Threads 'ListOfThreads AppState) `chain` continue)
    , Keybinding (EvKey KBS []) (scrollUp `chain` continue)
    , Keybinding (EvKey (KChar ' ') []) (scrollDown `chain` continue)
    ]
