module UI.Event.Main where

import qualified Brick.Main         as M
import qualified Brick.Types        as T
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty       as V
import           UI.Types

mainEvent :: AppState -> T.BrickEvent () e -> T.EventM () (T.Next AppState)
mainEvent a@(AppState s l Main) (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> M.halt a
        V.EvKey V.KEnter [] -> M.continue $ AppState s l ViewMail
        ev -> L.handleListEvent ev l >>= \l' -> M.continue $ AppState s l' Main
mainEvent l _ = M.continue l
