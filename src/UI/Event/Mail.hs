module UI.Event.Mail where

import qualified Brick.Main         as M
import qualified Brick.Types        as T
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty       as V
import           UI.Types

-- | The mail view shows a shortened list of mails. Forward all key strokes to
-- the list of mails by default.
mailEvent :: AppState -> T.BrickEvent () e -> T.EventM () (T.Next AppState)
mailEvent (AppState s l ViewMail) (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.continue $ AppState s l Main
    ev -> L.handleListEvent ev l >>= \l' -> M.continue $ AppState s l' ViewMail
mailEvent l _ = M.continue l
