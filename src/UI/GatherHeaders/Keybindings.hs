module UI.GatherHeaders.Keybindings where

import qualified Brick.Types as T
import qualified Graphics.Vty as V
import Types (Keybinding(..), AppState)
import UI.Actions

interactiveGatherHeadersKeybindings :: [Keybinding ctx (T.Next AppState)]
interactiveGatherHeadersKeybindings =
    [Keybinding (V.EvKey V.KEsc []) (backToIndex `chain` continue)]
