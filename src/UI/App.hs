{-# LANGUAGE DataKinds #-}
-- | The main application module
module UI.App where

import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Control.Lens.Getter (view)
import Control.Monad.Except (runExceptT)
import System.Exit (die)
import Data.Proxy

import Storage.Notmuch (getMessages)
import UI.Keybindings (dispatch)
import UI.ComposeEditor.Main (drawComposeEditor)
import UI.GatherHeaders.Main (drawInteractiveHeaders)
import UI.Index.Main (drawMain)
import UI.Actions (initialCompose)
import UI.Mail.Main (drawMail)
import UI.Help.Main (drawHelp)
import Types

drawUI :: AppState -> [Widget Name]
drawUI s =
    case view asAppMode s of
        BrowseMail -> drawMain s
        SearchMail -> drawMain s
        ManageTags -> drawMain s
        ViewMail -> drawMail s
        GatherHeadersFrom -> drawInteractiveHeaders s
        GatherHeadersTo -> drawInteractiveHeaders s
        GatherHeadersSubject -> drawInteractiveHeaders s
        ComposeEditor -> drawComposeEditor s
        Help -> drawHelp s

appEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
appEvent s (T.VtyEvent ev) =
  case view asAppMode s of
    BrowseMail -> dispatch (Proxy :: Proxy 'BrowseMail) s ev
    SearchMail -> dispatch (Proxy :: Proxy 'SearchMail) s ev
    ManageTags -> dispatch (Proxy :: Proxy 'ManageTags) s ev
    ViewMail -> dispatch (Proxy :: Proxy 'ViewMail) s ev
    GatherHeadersFrom -> dispatch (Proxy :: Proxy 'GatherHeadersFrom) s ev
    GatherHeadersTo -> dispatch (Proxy :: Proxy 'GatherHeadersTo) s ev
    GatherHeadersSubject -> dispatch (Proxy :: Proxy 'GatherHeadersSubject) s ev
    ComposeEditor -> dispatch (Proxy :: Proxy 'ComposeEditor) s ev
    Help -> dispatch (Proxy :: Proxy 'Help) s ev
appEvent s _ = M.continue s

initialState :: InternalConfiguration -> IO AppState
initialState conf = do
    let searchterms = view (confNotmuch . nmSearch) conf
    r <- runExceptT $ getMessages searchterms (view confNotmuch conf)
    case r of
      Left e -> die $ show e  -- TODO don't crash?
      Right vec ->
        let
          mi = MailIndex
                (L.list ListOfMails vec 1)
                (E.editor
                     EditorInput
                     Nothing
                     searchterms)
          mv = MailView Nothing Filtered
        in pure $ AppState conf mi mv initialCompose BrowseMail Nothing

theApp :: AppState -> M.App AppState e Name
theApp s =
    M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return
    , M.appAttrMap = const (view (asConfig . confColorMap) s)
    }
