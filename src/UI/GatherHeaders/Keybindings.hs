module UI.GatherHeaders.Keybindings where

import qualified Graphics.Vty   as V
import Types (Keybinding(..))
import UI.Actions

interactiveGatherHeadersKeybindings :: [Keybinding a]
interactiveGatherHeadersKeybindings =
    [Keybinding (V.EvKey V.KEsc []) backToIndex]
