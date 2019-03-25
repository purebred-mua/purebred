-- This file is part of purebred
-- Copyright (C) 2017 Róman Joost and Fraser Tweedale
--
-- purebred is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
import Control.Lens (set, to, view)
import qualified Data.Map as Map

import UI.Keybindings
import UI.GatherHeaders.Main (drawSubject, drawFrom, drawTo)
import UI.Index.Main
       (renderListOfThreads, renderListOfMails, renderSearchThreadsEditor,
        renderMailTagsEditor, renderThreadTagsEditor)
import UI.Actions (applySearch, initialCompose)
import UI.FileBrowser.Main
       (renderFileBrowser, renderFileBrowserSearchPathEditor)
import UI.Mail.Main
  ( renderAttachmentsList, renderMailAttachmentOpenWithEditor
  , renderMailAttachmentPipeToEditor, renderMailView)
import UI.Help.Main (renderHelp)
import UI.Status.Main (statusbar)
import UI.Views
       (indexView, mailView, composeView, helpView, listOfMailsView,
        filebrowserView, focusedViewWidget, focusedViewWidgets,
        focusedViewName)
import UI.ComposeEditor.Main (attachmentsEditor)
import Types

drawUI :: AppState -> [Widget Name]
drawUI s = [vBox (renderWidget s (focusedViewName s) <$> focusedViewWidgets s)]

renderWidget :: AppState -> ViewName -> Name -> Widget Name
renderWidget s _ ListOfThreads = renderListOfThreads s
renderWidget s ViewMail ListOfMails = vLimit (view (asConfig . confMailView . mvIndexRows) s) (renderListOfMails s)
renderWidget s _ MailAttachmentOpenWithEditor = renderMailAttachmentOpenWithEditor s
renderWidget s _ MailAttachmentPipeToEditor = renderMailAttachmentPipeToEditor s
renderWidget s _ ListOfMails = renderListOfMails s
renderWidget s _ ComposeListOfAttachments = attachmentsEditor s
renderWidget s _ MailListOfAttachments = renderAttachmentsList s
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
  f ComposeView ComposeListOfAttachments = dispatch eventHandlerComposeListOfAttachments
  f Mails ListOfMails = dispatch eventHandlerListOfMails
  f Mails ManageMailTagsEditor = dispatch eventHandlerManageMailTagsEditor
  f Threads ComposeFrom =  dispatch eventHandlerThreadComposeFrom
  f Threads ComposeSubject = dispatch eventHandlerThreadComposeSubject
  f Threads ComposeTo = dispatch eventHandlerThreadComposeTo
  f Threads ListOfThreads = dispatch eventHandlerListOfThreads
  f Threads ManageThreadTagsEditor = dispatch eventHandlerManageThreadTagsEditor
  f Threads SearchThreadsEditor = dispatch eventHandlerSearchThreadsEditor
  f ViewMail ManageMailTagsEditor = dispatch eventHandlerViewMailManageMailTagsEditor
  f ViewMail MailListOfAttachments = dispatch eventHandlerMailsListOfAttachments
  f ViewMail MailAttachmentOpenWithEditor = dispatch eventHandlerMailAttachmentOpenWithEditor
  f ViewMail MailAttachmentPipeToEditor = dispatch eventHandlerMailAttachmentPipeToEditor
  f ViewMail _ = dispatch eventHandlerScrollingMailView
  f _ ScrollingHelpView = dispatch eventHandlerScrollingHelpView
  f _ ListOfFiles = dispatch eventHandlerComposeFileBrowser
  f _ ManageFileBrowserSearchPath = dispatch eventHandlerManageFileBrowserSearchPath
  f _ _ = dispatch nullEventHandler


appEvent
  :: AppState               -- ^ program state
  -> T.BrickEvent Name PurebredEvent  -- ^ event
  -> T.EventM Name (T.Next AppState)
appEvent s (T.VtyEvent ev) = handleViewEvent (focusedViewName s) (focusedViewWidget s) s ev
appEvent s (T.AppEvent ev) = case ev of
  NotifyNumThreads n gen -> M.continue $
    if gen == view (asMailIndex . miListOfThreadsGeneration) s
    then set (asMailIndex . miThreads . listLength) (Just n) s
    else s
appEvent s _ = M.continue s

initialState :: InternalConfiguration -> IO AppState
initialState conf =
  let
    searchterms = view (confNotmuch . nmSearch) conf
    mi =
        MailIndex
            (ListWithLength (L.list ListOfMails mempty 1) (Just 0))
            (ListWithLength (L.list ListOfThreads mempty 1) (Just 0))
            firstGeneration
            (E.editorText SearchThreadsEditor Nothing searchterms)
            (E.editorText ManageMailTagsEditor Nothing "")
            (E.editorText ManageThreadTagsEditor Nothing "")
    mv = MailView
           Nothing
           Filtered
           (L.list MailListOfAttachments mempty 1)
           (E.editorText MailAttachmentOpenWithEditor Nothing "")
           (E.editorText MailAttachmentPipeToEditor Nothing "")
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
         (L.list ListOfFiles mempty 1)
         (E.editor ManageFileBrowserSearchPath Nothing path)
    mailboxes = view (confComposeView . cvIdentities) conf
    s = AppState conf mi mv (initialCompose mailboxes) Nothing viewsettings fb
  in
    applySearch s

theApp
  :: AppState               -- ^ initial state
  -> M.App AppState PurebredEvent Name
theApp s =
    M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return
    , M.appAttrMap = const (view (asConfig . confTheme . to themeToAttrMap) s)
    }
