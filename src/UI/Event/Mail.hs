-- | event handling for viewing a single mail
module UI.Event.Mail where

import qualified Brick.Main          as M
import qualified Brick.Types         as T
import qualified Brick.Widgets.List  as L
import           Control.Lens.Getter ((^.))
import           Control.Lens.Lens   ((&))
import           Control.Lens.Setter ((.~))
import qualified Graphics.Vty        as V
import           UI.Types

-- | The mail view shows a shortened list of mails. Forward all key strokes to
-- the list of mails by default.
mailEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
mailEvent s (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> M.continue $ asAppMode .~ Main $ s
        ev ->
            L.handleListEvent ev (s ^. asMailIndex ^. miListOfMails) >>=
            \l ->
                 M.continue $ s & asMailIndex . miListOfMails .~ l & asAppMode .~
                 ViewMail
mailEvent mi _ = M.continue mi
