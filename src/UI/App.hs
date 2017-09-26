-- | The main application module
module UI.App where

import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Control.Lens.Getter (view)
import Storage.Notmuch (getMessages)
import UI.ComposeEditor.Main (composeEditor, drawComposeEditor)
import UI.GatherHeaders.Main
       (drawInteractiveHeaders, interactiveGatherHeaders)
import UI.Index.Main (drawMain, mainEvent)
import UI.Keybindings (initialCompose)
import UI.Mail.Main (drawMail, mailEvent)
import Types

drawUI :: AppState -> [Widget Name]
drawUI s =
    case view asAppMode s of
        Main -> drawMain s
        ViewMail -> drawMail s
        GatherHeaders -> drawInteractiveHeaders s
        ComposeEditor -> drawComposeEditor s

appEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
appEvent s e =
    case view asAppMode s of
        Main -> mainEvent s e
        ViewMail -> mailEvent s e
        GatherHeaders -> interactiveGatherHeaders s e
        ComposeEditor -> composeEditor s e

initialState :: InternalConfiguration -> IO AppState
initialState conf = do
    let searchterms = view (confNotmuch . nmSearch) conf
    vec <- either error pure =<< getMessages searchterms (view confNotmuch conf)
    let mi =
            MailIndex
                (L.list ListOfMails vec 1)
                (E.editor
                     EditorInput
                     Nothing
                     searchterms)
                BrowseMail
    let mv = MailView Nothing Filtered
    return $ AppState conf mi mv initialCompose Main Nothing

theApp :: AppState -> M.App AppState e Name
theApp s =
    M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return
    , M.appAttrMap = const (view (asConfig . confColorMap) s)
    }
