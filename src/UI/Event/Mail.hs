-- | event handling for viewing a single mail
module UI.Event.Mail where

import qualified Brick.Main             as M
import           Graphics.Vty.Input.Events (Event)
import qualified Brick.Types            as T
import qualified Brick.Widgets.List     as L
import           Control.Lens.Getter    ((^.))
import           Control.Lens.Setter    ((.~))
import           Control.Monad.IO.Class (liftIO)
import           UI.Keybindings         (displayMailKeybindings,
                                         handleEvent,
                                         updateStateWithParsedMail)
import           UI.Types

-- | The mail view shows a shortened list of mails. Forward all key strokes to
-- the list of mails by default.
mailEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
mailEvent s ev = handleEvent displayMailKeybindings displayMailDefault s ev

displayMailDefault :: AppState -> Event -> T.EventM Name (T.Next AppState)
displayMailDefault s ev = do
            l' <- L.handleListEvent ev (s ^. asMailIndex ^. miListOfMails)
            s' <- liftIO $ updateStateWithParsedMail (asMailIndex . miListOfMails .~ l' $ s)
            M.continue s'
