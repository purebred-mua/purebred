-- | event handling for viewing a single mail
module UI.Event.Mail where

import qualified Brick.Main             as M
import qualified Brick.Types            as T
import qualified Brick.Widgets.List     as L
import           Control.Lens.Getter    ((^.))
import           Control.Lens.Setter    ((.~))
import           Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty           as V
import           UI.Event.Main          (updateStateWithParsedMail)
import           UI.Types

-- | The mail view shows a shortened list of mails. Forward all key strokes to
-- the list of mails by default.
mailEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
mailEvent s (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> M.continue $ asAppMode .~ Main $ s
        V.EvKey V.KBS [] -> scrollMailViewPage s T.Up
        V.EvKey (V.KChar ' ') [] -> scrollMailViewPage s T.Down
        ev -> do
            l' <- L.handleListEvent ev (s ^. asMailIndex ^. miListOfMails)
            s' <- liftIO $ updateStateWithParsedMail (asMailIndex . miListOfMails .~ l' $ s)
            M.continue s'
mailEvent mi _ = M.continue mi


scrollMailViewPage :: AppState -> T.Direction -> T.EventM Name (T.Next AppState)
scrollMailViewPage s d = do
  let vp = M.viewportScroll ScrollingMailView
  M.vScrollPage vp d
  M.continue s
