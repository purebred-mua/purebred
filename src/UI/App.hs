{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The main application module
module UI.App where

import qualified Brick.Main as M
import Brick.Types (Widget)
import Brick.Focus (focusRing)
import Brick.Widgets.Core (vBox, vLimit)
import Brick.Themes (themeToAttrMap)
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty.Input.Events as Vty
import Control.Lens (to, view)
import Control.Monad.Except (runExceptT)
import qualified Data.Vector as Vector
import System.Exit (die)
import qualified Data.Map as Map

import Storage.Notmuch (getThreads)
import UI.Keybindings
import UI.GatherHeaders.Main (drawSubject, drawFrom, drawTo)
import UI.Index.Main
       (renderListOfThreads, renderListOfMails, renderSearchThreadsEditor,
        renderMailTagsEditor, renderThreadTagsEditor)
import UI.Actions (initialCompose)
import UI.FileBrowser.Main
       (renderFileBrowser, renderFileBrowserSearchPathEditor)
import UI.Mail.Main (renderMailView)
import UI.Help.Main (renderHelp)
import UI.Status.Main (statusbar)
import UI.Utils (focusedViewWidget, focusedViewWidgets, focusedViewName)
import UI.Views
       (indexView, mailView, composeView, helpView, listOfMailsView,
        filebrowserView)
import UI.ComposeEditor.Main (attachmentsEditor)
import Types

drawUI :: AppState -> [Widget Name]
drawUI s = [vBox (renderWidget s (focusedViewName s) <$> focusedViewWidgets s)]

renderWidget :: AppState -> ViewName -> Name -> Widget Name
renderWidget s _ ListOfThreads = renderListOfThreads s
renderWidget s ViewMail ListOfMails = vLimit (view (asConfig . confMailView . mvIndexRows) s) (renderListOfMails s)
renderWidget s _ ListOfMails = renderListOfMails s
renderWidget s _ ListOfAttachments = attachmentsEditor s
renderWidget s _ ListOfFiles = renderFileBrowser s
renderWidget s _ ManageFileBrowserSearchPath = renderFileBrowserSearchPathEditor s
renderWidget s _ SearchThreadsEditor = renderSearchThreadsEditor s
renderWidget s _ ManageMailTagsEditor = renderMailTagsEditor s
renderWidget s _ ManageThreadTagsEditor = renderThreadTagsEditor s
renderWidget s _ ScrollingMailView = renderMailView s
renderWidget s _ ScrollingHelpView = renderHelp s
renderWidget s _ ComposeFrom = drawFrom s
renderWidget s _ ComposeTo = drawTo s
renderWidget s _ ComposeSubject = drawSubject s
renderWidget s _ StatusBar = statusbar s

handleViewEvent :: ViewName -> Name -> AppState -> Vty.Event -> T.EventM Name (T.Next AppState)
handleViewEvent = f where
  f ComposeView ComposeFrom = dispatch eventHandlerComposeFrom
  f ComposeView ComposeSubject = dispatch eventHandlerComposeSubject
  f ComposeView ComposeTo = dispatch eventHandlerComposeTo
  f ComposeView ListOfAttachments = dispatch eventHandlerComposeListOfAttachments
  f Mails ListOfMails = dispatch eventHandlerListOfMails
  f Mails ManageMailTagsEditor = dispatch eventHandlerManageMailTagsEditor
  f Threads ComposeFrom =  dispatch eventHandlerThreadComposeFrom
  f Threads ComposeSubject = dispatch eventHandlerThreadComposeSubject
  f Threads ComposeTo = dispatch eventHandlerThreadComposeTo
  f Threads ListOfThreads = dispatch eventHandlerListOfThreads
  f Threads ManageThreadTagsEditor = dispatch eventHandlerManageThreadTagsEditor
  f Threads SearchThreadsEditor = dispatch eventHandlerSearchThreadsEditor
  f ViewMail ManageMailTagsEditor = dispatch eventHandlerViewMailManageMailTagsEditor
  f ViewMail _ = dispatch eventHandlerScrollingMailView
  f _ ScrollingHelpView = dispatch eventHandlerScrollingHelpView
  f _ ListOfFiles = dispatch eventHandlerComposeFileBrowser
  f _ ManageFileBrowserSearchPath = dispatch eventHandlerManageFileBrowserSearchPath
  f _ _ = dispatch nullEventHandler


appEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
appEvent s (T.VtyEvent ev) = handleViewEvent (focusedViewName s) (focusedViewWidget s ListOfThreads) s ev
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
                          , (ComposeView, composeView)
                          , (FileBrowser, filebrowserView)]
                    , _vsFocusedView = focusRing [Threads, Mails, ViewMail, Help, ComposeView, FileBrowser]
                    }
                path = view (confFileBrowserView . fbHomePath) conf
                fb = CreateFileBrowser
                     (L.list ListOfFiles Vector.empty 1)
                     (E.editor ManageFileBrowserSearchPath Nothing path)
            in pure $
               AppState conf mi mv initialCompose Nothing viewsettings fb

theApp :: AppState -> M.App AppState e Name
theApp s =
    M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return
    , M.appAttrMap = const (view (asConfig . confTheme . to themeToAttrMap) s)
    }
