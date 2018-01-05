{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The main application module
module UI.App where

import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Control.Lens.Getter (view)
import Control.Monad.Except (runExceptT)
import qualified Data.Vector as Vector
import System.Exit (die)
import Data.Proxy

import Storage.Notmuch (getThreads)
import UI.Keybindings (dispatch)
import UI.ComposeEditor.Main (drawComposeEditor)
import UI.GatherHeaders.Main (drawInteractiveHeaders)
import UI.Index.Main (drawMain)
import UI.Actions (initialCompose)
import UI.Mail.Main (drawMail)
import UI.Help.Main (drawHelp)
import UI.FileBrowser.Main (drawFileBrowser)
import Types

drawUI :: AppState -> [Widget Name]
drawUI s =
    case view asAppMode s of
        BrowseMail -> drawMain s
        BrowseThreads -> drawMain s
        SearchThreads -> drawMain s
        ManageMailTags -> drawMain s
        ManageThreadTags -> drawMain s
        ViewMail -> drawMail s
        GatherHeadersFrom -> drawInteractiveHeaders s
        GatherHeadersTo -> drawInteractiveHeaders s
        GatherHeadersSubject -> drawInteractiveHeaders s
        ComposeEditor -> drawComposeEditor s
        Help -> drawHelp s
        AddAttachment -> drawFileBrowser s

appEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
appEvent s (T.VtyEvent ev) =
  case view asAppMode s of
    BrowseMail -> dispatch (Proxy :: Proxy 'BrowseMail) s ev
    BrowseThreads -> dispatch (Proxy :: Proxy 'BrowseThreads) s ev
    SearchThreads -> dispatch (Proxy :: Proxy 'SearchThreads) s ev
    ManageMailTags -> dispatch (Proxy :: Proxy 'ManageMailTags) s ev
    ManageThreadTags -> dispatch (Proxy :: Proxy 'ManageThreadTags) s ev
    ViewMail -> dispatch (Proxy :: Proxy 'ViewMail) s ev
    GatherHeadersFrom -> dispatch (Proxy :: Proxy 'GatherHeadersFrom) s ev
    GatherHeadersTo -> dispatch (Proxy :: Proxy 'GatherHeadersTo) s ev
    GatherHeadersSubject -> dispatch (Proxy :: Proxy 'GatherHeadersSubject) s ev
    ComposeEditor -> dispatch (Proxy :: Proxy 'ComposeEditor) s ev
    Help -> dispatch (Proxy :: Proxy 'Help) s ev
    AddAttachment -> dispatch (Proxy :: Proxy 'AddAttachment) s ev
appEvent s _ = M.continue s

initialState :: InternalConfiguration -> IO AppState
initialState conf = do
    let searchterms = view (confNotmuch . nmSearch) conf
    r <- runExceptT $ getThreads searchterms (view confNotmuch conf)
    case r of
      Left e -> die $ show e  -- TODO don't crash?
      Right vec ->
        let
          mi = MailIndex
                (L.list ListOfMails Vector.empty 1)
                (L.list ListOfThreads vec 1)
                (E.editorText SearchThreadsEditor Nothing searchterms)
                (E.editorText ManageMailTagsEditor Nothing "")
                (E.editorText ManageThreadTagsEditor Nothing "")
          mv = MailView Nothing Filtered
          bf = BrowseFiles (L.list ListOfFiles Vector.empty 1) ""
        in pure $ AppState conf mi mv initialCompose BrowseThreads Nothing bf

theApp :: AppState -> M.App AppState e Name
theApp s =
    M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return
    , M.appAttrMap = const (view (asConfig . confColorMap) s)
    }
