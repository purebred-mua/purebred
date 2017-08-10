module UI.GatherHeaders.Keybindings where

import qualified Graphics.Vty   as V
import           UI.Keybindings (cancelToMain)
import           UI.Types       (Keybinding (..))

interactiveGatherHeadersKeybindings :: [Keybinding]
interactiveGatherHeadersKeybindings =
    [Keybinding "Return to list of mails" (V.EvKey V.KEsc []) cancelToMain]
