module UI.Mail.Keybindings where

import qualified Brick.Main          as M
import qualified Brick.Types         as T
import Control.Lens.Setter (set)
import Control.Lens.Getter (view)
import qualified Graphics.Vty        as V
import Types

displayMailKeybindings :: [Keybinding]
displayMailKeybindings =
  [ Keybinding "Return to list of mails" (V.EvKey V.KEsc []) (\s -> M.continue $ set asAppMode Main $ s)
  , Keybinding "Scroll e-mail up" (V.EvKey V.KBS []) (\s -> scrollMailViewPage s T.Up)
  , Keybinding "Scroll e-mail down" (V.EvKey (V.KChar ' ') []) (\s -> scrollMailViewPage s T.Down)
  , Keybinding "toggle between filtered and all Headers" (V.EvKey (V.KChar 'h') []) (M.continue . toggleHeaders)
  ]

scrollMailViewPage :: AppState -> T.Direction -> T.EventM Name (T.Next AppState)
scrollMailViewPage s d = do
  let vp = M.viewportScroll ScrollingMailView
  M.vScrollPage vp d
  M.continue s

toggleHeaders :: AppState -> AppState
toggleHeaders s = case view (asMailView . mvHeadersState) s of
  Filtered -> set (asMailView . mvHeadersState) ShowAll s
  ShowAll -> set (asMailView . mvHeadersState) Filtered s
