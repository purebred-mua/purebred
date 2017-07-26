-- | event handling for viewing a single mail
module UI.Event.Mail where

import qualified Brick.Main         as M
import qualified Brick.Types        as T
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty       as V
import           Lens.Micro         ((^.))
import           UI.Types

-- | The mail view shows a shortened list of mails. Forward all key strokes to
-- the list of mails by default.
-- TODO: Yikes!! Use lenses for setting the state ...
mailEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
mailEvent (AppState s db mi ViewMail) (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> M.continue $ AppState s db mi Main
        ev ->
            L.handleListEvent ev (mi ^. listOfMails) >>=
            \mi' ->
                 M.continue $
                 AppState s db (MailIndex mi' (mi ^. searchEditor) (mi^.miMode)) ViewMail
mailEvent mi _ = M.continue mi
