-- | The main application module
{-# LANGUAGE OverloadedStrings #-}
module UI.App where

import qualified Brick.Main            as M
import           Brick.Types           (Widget)
import qualified Brick.Types           as T
import qualified Brick.Widgets.Edit    as E
import qualified Brick.Widgets.List    as L
import           Control.Lens.Getter   ((^.))
import           Storage.Notmuch       (getMessages)
import           UI.ComposeEditor.Main (composeEditor, drawComposeEditor)
import           UI.GatherHeaders.Main (drawInteractiveHeaders,
                                        interactiveGatherHeaders)
import           UI.Index.Main         (drawMain, mainEvent)
import           UI.Keybindings        (initialCompose)
import           UI.Mail.Main          (drawMail, mailEvent)
import           UI.Types

drawUI :: AppState -> [Widget Name]
drawUI s =
    case s ^. asAppMode of
        Main -> drawMain s
        ViewMail -> drawMail s
        GatherHeaders -> drawInteractiveHeaders s
        ComposeEditor -> drawComposeEditor s

appEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
appEvent s e =
    case s ^. asAppMode of
        Main -> mainEvent s e
        ViewMail -> mailEvent s e
        GatherHeaders -> interactiveGatherHeaders s e
        ComposeEditor -> composeEditor s e

initialState :: Configuration -> IO AppState
initialState conf = do
    let searchterms = conf ^. confNotmuch ^. nmSearch
    vec <- getMessages searchterms (conf ^. confNotmuch)
    let mi =
            MailIndex
                (L.list ListOfMails vec 1)
                (E.editor
                     EditorInput
                     Nothing
                     searchterms)
                BrowseMail
    let mv = MailView Nothing
    return $ AppState conf mi mv initialCompose Main Nothing

theApp :: AppState -> M.App AppState e Name
theApp s =
    M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return
    , M.appAttrMap = const (s ^. asConfig ^. confColorMap)
    }
