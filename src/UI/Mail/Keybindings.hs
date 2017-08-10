module UI.Mail.Keybindings where

import qualified Brick.Main          as M
import qualified Brick.Types         as T
import           Control.Lens.Setter (set)
import qualified Graphics.Vty        as V
import           UI.Types

displayMailKeybindings :: [Keybinding]
displayMailKeybindings =
  [ Keybinding "Return to list of mails" (V.EvKey V.KEsc []) (\s -> M.continue $ set asAppMode Main $ s)
  , Keybinding "Scroll e-mail up" (V.EvKey V.KBS []) (\s -> scrollMailViewPage s T.Up)
  , Keybinding "Scroll e-mail down" (V.EvKey (V.KChar ' ') []) (\s -> scrollMailViewPage s T.Down)
  ]

scrollMailViewPage :: AppState -> T.Direction -> T.EventM Name (T.Next AppState)
scrollMailViewPage s d = do
  let vp = M.viewportScroll ScrollingMailView
  M.vScrollPage vp d
  M.continue s
