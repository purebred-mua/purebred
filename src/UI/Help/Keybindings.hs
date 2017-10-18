{-# LANGUAGE DataKinds #-}

module UI.Help.Keybindings where

import qualified Brick.Types as Brick
import UI.Keybindings
import Types

-- | Default Keybindings
helpKeybindings :: [Keybinding 'Help (Brick.Next AppState)]
helpKeybindings = scrollableKeybindings
