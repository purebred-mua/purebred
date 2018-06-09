{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The main application module
module UI.App where

import qualified Brick.Main as M
import Brick.Types (Widget)
import Brick.Focus (focusRing)
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty.Input.Events as Vty
import Control.Lens.Getter (view)
import Control.Monad.Except (runExceptT)
import qualified Data.Vector as Vector
import System.Exit (die)
import Data.Proxy
import qualified Data.Map as Map

import Storage.Notmuch (getThreads)
import UI.Keybindings (dispatch)
import UI.GatherHeaders.Main (drawSubject, drawFrom, drawTo)
import UI.Index.Main
       (renderListOfThreads, renderListOfMails, renderSearchThreadsEditor,
        renderMailTagsEditor, renderThreadTagsEditor)
import UI.Actions (initialCompose)
import UI.Mail.Main (renderMailView)
import UI.Help.Main (renderHelp)
import UI.Status.Main (statusbar)
import UI.Utils (focusedViewWidget, focusedViewWidgets, focusedViewName)
import UI.Views (indexView, mailView, composeView, helpView, listOfMailsView)
import UI.ComposeEditor.Main (attachmentsEditor)
import Types

drawUI :: AppState -> [Widget Name]
drawUI s = [unVBox $ foldMap (VBox . renderWidget s) (focusedViewWidgets s)]

renderWidget :: AppState -> Name -> Widget Name
renderWidget s ListOfThreads = renderListOfThreads s
renderWidget s ListOfMails = renderListOfMails s
renderWidget s ListOfAttachments = attachmentsEditor s
renderWidget s SearchThreadsEditor = renderSearchThreadsEditor s
renderWidget s ManageMailTagsEditor = renderMailTagsEditor s
renderWidget s ManageThreadTagsEditor = renderThreadTagsEditor s
renderWidget s ScrollingMailView = renderMailView s
renderWidget s ScrollingHelpView = renderHelp s
renderWidget s ComposeFrom = drawFrom s
renderWidget s ComposeTo = drawTo s
renderWidget s ComposeSubject = drawSubject s
renderWidget s StatusBar = statusbar s

handleViewEvent :: ViewName -> Name -> AppState -> Vty.Event -> T.EventM Name (T.Next AppState)
handleViewEvent ComposeView ComposeFrom s ev =  dispatch (Proxy :: Proxy 'ComposeView) (Proxy :: Proxy 'ComposeFrom) s ev
handleViewEvent ComposeView ComposeTo s ev = dispatch (Proxy :: Proxy 'ComposeView) (Proxy :: Proxy 'ComposeTo) s ev
handleViewEvent ComposeView ComposeSubject s ev = dispatch (Proxy :: Proxy 'ComposeView) (Proxy :: Proxy 'ComposeSubject) s ev
handleViewEvent Threads ComposeFrom s ev =  dispatch (Proxy :: Proxy 'Threads) (Proxy :: Proxy 'ComposeFrom) s ev
handleViewEvent Threads ComposeTo s ev = dispatch (Proxy :: Proxy 'Threads) (Proxy :: Proxy 'ComposeTo) s ev
handleViewEvent Threads ComposeSubject s ev = dispatch (Proxy :: Proxy 'Threads) (Proxy :: Proxy 'ComposeSubject) s ev
handleViewEvent ViewMail ScrollingMailView s ev =  dispatch (Proxy :: Proxy 'ViewMail) (Proxy :: Proxy 'ScrollingMailView) s ev
handleViewEvent ViewMail ManageMailTagsEditor s ev =  dispatch (Proxy :: Proxy 'ViewMail) (Proxy :: Proxy 'ManageMailTagsEditor) s ev
handleViewEvent _ _ s _ = M.continue s


appEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
appEvent s (T.VtyEvent ev) =
  case focusedViewWidget s ListOfThreads of
    ListOfMails -> dispatch (Proxy :: Proxy 'Mails) (Proxy :: Proxy 'ListOfMails) s ev
    ListOfThreads -> dispatch (Proxy :: Proxy 'Threads) (Proxy :: Proxy 'ListOfThreads) s ev
    ListOfAttachments -> dispatch (Proxy :: Proxy 'ComposeView) (Proxy :: Proxy 'ListOfAttachments) s ev
    SearchThreadsEditor -> dispatch (Proxy :: Proxy 'Threads) (Proxy :: Proxy 'SearchThreadsEditor) s ev
    ManageMailTagsEditor -> dispatch (Proxy :: Proxy 'Mails) (Proxy :: Proxy 'ManageMailTagsEditor) s ev
    ManageThreadTagsEditor -> dispatch (Proxy :: Proxy 'Threads) (Proxy :: Proxy 'ManageThreadTagsEditor) s ev
    ScrollingMailView -> dispatch (Proxy :: Proxy 'ViewMail) (Proxy :: Proxy 'ScrollingMailView) s ev
    ComposeFrom -> handleViewEvent (focusedViewName s) ComposeFrom s ev
    ComposeTo -> handleViewEvent (focusedViewName s) ComposeTo s ev
    ComposeSubject -> handleViewEvent (focusedViewName s) ComposeSubject s ev
    ScrollingHelpView -> dispatch (Proxy :: Proxy 'Help) (Proxy :: Proxy 'ScrollingHelpView) s ev
    StatusBar -> M.continue s -- can not be focused for input
appEvent s _ = M.continue s

initialState :: InternalConfiguration -> IO AppState
initialState conf = do
    let searchterms = view (confNotmuch . nmSearch) conf
    r <- runExceptT $ getThreads searchterms (view confNotmuch conf)
    case r of
        Left e -> die $ show e  -- TODO don't crash?
        Right vec ->
            let mi =
                    MailIndex
                        (L.list ListOfMails Vector.empty 1)
                        (L.list ListOfThreads vec 1)
                        (E.editorText SearchThreadsEditor Nothing searchterms)
                        (E.editorText ManageMailTagsEditor Nothing "")
                        (E.editorText ManageThreadTagsEditor Nothing "")
                mv = MailView Nothing Filtered
                viewsettings =
                    ViewSettings
                    { _vsViews = Map.fromList
                          [ (Threads, indexView)
                          , (Mails, listOfMailsView)
                          , (ViewMail, mailView)
                          , (Help, helpView)
                          , (ComposeView, composeView)]
                    , _vsFocusedView = focusRing [Threads, Mails, ViewMail, Help, ComposeView]
                    }
            in pure $
               AppState conf mi mv initialCompose Nothing viewsettings

theApp :: AppState -> M.App AppState e Name
theApp s =
    M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return
    , M.appAttrMap = const (view (asConfig . confColorMap) s)
    }
