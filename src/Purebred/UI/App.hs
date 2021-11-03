-- This file is part of purebred
-- Copyright (C) 2017-2021 Róman Joost and Fraser Tweedale
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
{-# LANGUAGE TypeApplications #-}

module Purebred.UI.App where

import qualified Brick.Main as M
import Brick.Types (Widget)
import Brick.Focus (focusRing)
import Brick.Widgets.Core (vBox, vLimit)
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.FileBrowser as FB
import qualified Graphics.Vty.Input.Events as Vty
import Control.Lens (set, view)
import Control.Monad.State (execStateT)
import qualified Data.Map as Map
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (fromGregorian)
import Data.Proxy

import Purebred.UI.Keybindings
import Purebred.UI.Index.Main
import Purebred.UI.Actions (applySearch, initialCompose)
import Purebred.UI.FileBrowser.Main
       (renderFileBrowser, renderFileBrowserSearchPathEditor)
import Purebred.UI.Mail.Main (renderAttachmentsList, renderMailView)
import Purebred.UI.Help.Main (renderHelp)
import Purebred.UI.Status.Main (statusbar)
import Purebred.UI.Views
       (indexView, mailView, composeView, helpView,
        filebrowserView, focusedViewWidget, visibleViewWidgets,
        focusedViewName)
import Purebred.UI.ComposeEditor.Main (attachmentsEditor, drawHeaders, renderConfirm)
import Purebred.UI.Draw.Main (renderEditorWithLabel)
import Purebred.Types
import Purebred.UI.Widgets (statefulEditor)

-- * Synopsis
--
-- $synopsis
-- This module ties in all functions for rendering and handling events.
--

-- ** Differences to Brick
-- $differences
-- Purebred uses Brick widgets, but in order to make Purebred
-- configurable, we've made changes to how we use Brick. The single
-- difference to Brick is found on how we process keys (see
-- 'Purebred.UI.Keybindings'). Brick handles keys directly in the
-- widget. Purebred instead looks up keybindings first. If nothing
-- matches, the key is forwarded to the widget.

-- | Main UI drawing function. Looks up which widgets need to be
-- rendered in the current 'View' and traverses each layer pattern
-- matching the 'ViewName' and widget 'Name' in 'renderWidget' to draw
-- the widget.
--
drawUI :: AppState -> [Widget Name]
drawUI s = vBox . fmap (renderWidget s (focusedViewName s)) <$> visibleViewWidgets s

renderWidget :: AppState -> ViewName -> Name -> Widget Name
renderWidget s _ ListOfThreads = renderListOfThreads s
renderWidget s ViewMail ListOfMails = vLimit (view (asConfig . confMailView . mvIndexRows) s) (renderListOfMails s)
renderWidget s _ MailAttachmentOpenWithEditor =
  renderEditorWithLabel (Proxy @'MailAttachmentOpenWithEditor) "Open with:" s
renderWidget s _ MailAttachmentPipeToEditor =
  renderEditorWithLabel (Proxy @'MailAttachmentPipeToEditor) "Pipe to:" s
renderWidget s _ ListOfMails = renderListOfMails s
renderWidget s _ ComposeListOfAttachments = attachmentsEditor s
renderWidget s _ MailListOfAttachments = renderAttachmentsList s
renderWidget s _ ListOfFiles = renderFileBrowser s
renderWidget s _ ManageFileBrowserSearchPath = renderFileBrowserSearchPathEditor s
renderWidget s _ SaveToDiskPathEditor =
  renderEditorWithLabel (Proxy @'SaveToDiskPathEditor) "Save to file:" s
renderWidget s _ SearchThreadsEditor =
  renderEditorWithLabel (Proxy @'SearchThreadsEditor) "Query:" s
renderWidget s _ ManageMailTagsEditor =
  renderEditorWithLabel (Proxy @'ManageMailTagsEditor) "Labels:" s
renderWidget s _ ManageThreadTagsEditor =
  renderEditorWithLabel (Proxy @'ManageThreadTagsEditor) "Labels:" s
renderWidget s _ ScrollingMailView = renderMailView s
renderWidget s _ ScrollingMailViewFindWordEditor =
  renderEditorWithLabel (Proxy @'ScrollingMailViewFindWordEditor) "Search for:" s
renderWidget s _ ScrollingHelpView = renderHelp s
renderWidget s _ ComposeFrom = renderEditorWithLabel (Proxy @'ComposeFrom) "From:" s
renderWidget s _ ComposeTo = renderEditorWithLabel (Proxy @'ComposeTo) "To:" s
renderWidget s _ ComposeCc = renderEditorWithLabel (Proxy @'ComposeCc) "Cc:" s
renderWidget s _ ComposeBcc = renderEditorWithLabel (Proxy @'ComposeBcc) "Bcc:" s
renderWidget s _ ComposeSubject = renderEditorWithLabel (Proxy @'ComposeSubject) "Subject:" s
renderWidget s _ ComposeHeaders = drawHeaders s
renderWidget s _ StatusBar = statusbar s
renderWidget s _ ConfirmDialog = renderConfirm s

-- | Main event handler
--
handleViewEvent :: ViewName -> Name -> AppState -> Vty.Event -> T.EventM Name (T.Next AppState)
handleViewEvent = f where
  f ComposeView ComposeFrom = dispatch eventHandlerComposeFrom
  f ComposeView ComposeSubject = dispatch eventHandlerComposeSubject
  f ComposeView ComposeTo = dispatch eventHandlerComposeTo
  f ComposeView ComposeCc = dispatch eventHandlerComposeCc
  f ComposeView ComposeBcc = dispatch eventHandlerComposeBcc
  f ComposeView ComposeListOfAttachments = dispatch eventHandlerComposeListOfAttachments
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
  f ViewMail ScrollingMailViewFindWordEditor = dispatch eventHandlerScrollingMailViewFind
  f ViewMail SaveToDiskPathEditor = dispatch eventHandlerSaveToDiskEditor
  f ViewMail ComposeTo = dispatch eventHandlerViewMailComposeTo
  f ViewMail _ = dispatch eventHandlerScrollingMailView
  f _ ScrollingHelpView = dispatch eventHandlerScrollingHelpView
  f _ ListOfFiles = dispatch eventHandlerComposeFileBrowser
  f _ ManageFileBrowserSearchPath = dispatch eventHandlerManageFileBrowserSearchPath
  f _ ConfirmDialog = dispatch eventHandlerConfirm
  f _ _ = dispatch nullEventHandler


-- | Handling of application events. These can be keys which are
-- pressed by the user or asynchronous events send by threads.
--
appEvent ::
     AppState
  -> T.BrickEvent Name PurebredEvent -- ^ event
  -> T.EventM Name (T.Next AppState)
appEvent s (T.VtyEvent ev) = handleViewEvent (focusedViewName s) (focusedViewWidget s) s ev
appEvent s (T.AppEvent ev) = case ev of
  NotifyNumThreads n gen -> M.continue $
    if gen == view (asThreadsView . miListOfThreadsGeneration) s
    then set (asThreadsView . miThreads . listLength) (Just n) s
    else s
  NotifyNewMailArrived n -> M.continue (set (asThreadsView . miNewMail) n s)
  InputValidated err -> M.continue . ($ s) $
    case err of
      -- No error at all. Clear any existing errors set in the state.
      Nothing -> set asUserMessage Nothing . set (asAsync . aValidation) Nothing
      (Just msg) ->
        let allVisible = concat $ visibleViewWidgets s
            widget = view umContext msg
         in if widget `elem` allVisible
               -- Widget for this message is still visible, display
               -- the message and clear the thread ID.
              then set asUserMessage err . set (asAsync . aValidation) Nothing
               -- Widget for this message is hidden, ignore the
               -- message, clear existing message and thread states.
              else set asUserMessage Nothing . set (asAsync . aValidation) Nothing
appEvent s _ = M.continue s

initialState :: InternalConfiguration -> IO AppState
initialState conf = do
  fb' <- FB.newFileBrowser
         FB.selectNonDirectories
         ListOfFiles
         (Just $ view (confFileBrowserView . fbHomePath) conf)
  let
    searchterms = view (confNotmuch . nmSearch) conf
    mi =
        ThreadsView
            (ListWithLength (L.list ListOfMails mempty 1) (Just 0))
            (ListWithLength (L.list ListOfThreads mempty 1) (Just 0))
            firstGeneration
            (statefulEditor $ E.editorText SearchThreadsEditor Nothing searchterms)
            (E.editorText ManageMailTagsEditor Nothing "")
            (E.editorText ManageThreadTagsEditor Nothing "")
            0
    mv = MailView
           Nothing
           (MailBody mempty [])
           Filtered
           (L.list MailListOfAttachments mempty 1)
           (E.editorText SaveToDiskPathEditor Nothing "")
           (E.editorText MailAttachmentOpenWithEditor Nothing "")
           (E.editorText MailAttachmentPipeToEditor Nothing "")
           (E.editorText ScrollingMailViewFindWordEditor Nothing "")
           (focusRing [])
    viewsettings =
        ViewSettings
        { _vsViews = Map.fromList
              [ (Threads, indexView)
              , (ViewMail, mailView)
              , (Help, helpView)
              , (ComposeView, composeView)
              , (FileBrowser, filebrowserView)]
        , _vsFocusedView = focusRing [Threads, Mails, ViewMail, Help, ComposeView, FileBrowser]
        }
    path = view (confFileBrowserView . fbHomePath) conf
    fb = CreateFileBrowser
         fb'
         (statefulEditor $ E.editor ManageFileBrowserSearchPath Nothing path)
    mailboxes = view (confComposeView . cvIdentities) conf
    epoch = UTCTime (fromGregorian 2018 07 18) 1
    async = Async Nothing
    s = AppState conf mi mv (initialCompose mailboxes) Nothing viewsettings fb epoch async
  execStateT applySearch s

-- | Application event loop.
theApp ::
     AppState -- ^ initial state
  -> M.App AppState PurebredEvent Name
theApp s =
    M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return
    , M.appAttrMap = const (view (asConfig . confTheme) s)
    }
