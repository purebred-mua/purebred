module UI.Event.Mail where

import qualified Brick.Main   as M
import qualified Brick.Types  as T
import qualified Graphics.Vty as V
import           UI.Types

mailEvent :: AppState -> T.BrickEvent () e -> T.EventM () (T.Next AppState)
mailEvent (AppState s l ViewMail) (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.continue $ AppState s l Main
    _ -> M.continue $ AppState s l ViewMail
mailEvent l _ = M.continue l
