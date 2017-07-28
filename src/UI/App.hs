-- | The main application module
{-# LANGUAGE OverloadedStrings #-}
module UI.App where

import qualified Brick.Main          as M
import           Brick.Types         (Widget)
import qualified Brick.Types         as T
import qualified Brick.Widgets.Edit  as E
import qualified Brick.Widgets.List  as L
import           Control.Lens.Getter ((^.))
import qualified Data.Text           as T
import           Storage.Notmuch     (getMessages)
import           UI.Draw.Mail        (drawMail)
import           UI.Draw.Main        (drawMain, theMap)
import           UI.Event.Mail       (mailEvent)
import           UI.Event.Main       (mainEvent)
import           UI.Types

drawUI :: AppState -> [Widget Name]
drawUI s =
    case s ^. asAppMode of
        Main -> drawMain s
        ViewMail -> drawMail s

appEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
appEvent s e =
    case s ^. asAppMode of
        Main -> mainEvent s e
        ViewMail -> mailEvent s e

initialState :: String -> IO AppState
initialState dbfp = do
    let searchterms = "tag:inbox"
    vec <- getMessages dbfp searchterms
    let mi =
            MailIndex
                (L.list ListOfMails vec 1)
                (E.editor
                     EditorInput
                     Nothing
                     (T.pack searchterms))
                BrowseMail
    let mv = MailView Nothing
    return $ AppState searchterms dbfp mi mv Main Nothing

theApp :: M.App AppState e Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }
