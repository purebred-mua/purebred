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
{-# LANGUAGE LambdaCase #-}

module Purebred.UI.App
  ( theApp
  , initialState
  , initialViews
  ) where

import Control.Lens (prism, assign, use, view)
import Control.Monad (when)
import Control.Concurrent.STM (TMVar, readTVarIO, writeTVar, newTVarIO, tryTakeTMVar, putTMVar, newEmptyTMVarIO)
import Control.Monad.State (get, gets, execStateT)
import qualified Data.Map as Map
import qualified Data.Text.Lazy as T
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (fromGregorian)
import Data.Proxy
import qualified Data.Text
import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline (InputT, getInputLineWithInitial)
import GHC.Conc (TVar, atomically)

import Brick
  ( App(..), BrickEvent(..), EventM, Widget
  , emptyWidget, showFirstCursor, vBox, vLimit
  )
import Brick.BChan (BChan, writeBChan)
import Brick.Focus (focusRing)
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.FileBrowser as FB
import qualified Brick.Main as M
import qualified Graphics.Vty.Input.Events as Vty

import Purebred.Storage.Server
import Purebred.Types
import Purebred.Types.Presentation (MatchInfo(NoSearch), emptyBodyPresentation)
import Purebred.UI.Keybindings
import Purebred.UI.Index.Main
import Purebred.UI.Actions (runSearch, initialCompose)
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
import Purebred.UI.Draw.Main (renderEditorWithLabel, renderHaskeline)
import Purebred.UI.Widgets (statefulEditor)
import qualified Brick.Haskeline as HB

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
  renderHaskeline "Open with:" (view (asMailView . mvOpenCommand) s)
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
  renderHaskeline "Query:" (view (asThreadsView . miSearchThreadsEditor) s)
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
renderWidget _ _ _ =
  -- this case handles symbolic Names that don't represent a
  -- top-level widget (e.g. dialog buttons)
  emptyWidget

-- | Main event handler
--
handleViewEvent :: ViewName -> Name -> Vty.Event -> EventM Name AppState ()
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
appEvent
  :: BrickEvent Name PurebredEvent -- ^ event
  -> EventM Name AppState ()
appEvent (VtyEvent ev) = do
  s <- get
  handleViewEvent (focusedViewName s) (focusedViewWidget s) ev
appEvent appev@(AppEvent ev) =
  case ev of
    FromHBWidget tb ->
      case view HB.tbNameL tb of
        SearchThreadsEditor -> T.zoom (asThreadsView . miSearchThreadsEditor) (HB.handleAppEvent appev)
        MailAttachmentOpenWithEditor -> T.zoom (asMailView . mvOpenCommand) (HB.handleAppEvent appev)
        _ -> pure ()
    FromHaskeline _ -> pure ()
    HaskelineDied _ -> M.halt
    NotifyNumThreads n gen -> do
        curGen <- use (asThreadsView . miListOfThreadsGeneration)
        when (gen == curGen) $
          assign (asThreadsView . miThreads . listLength) (Just n)
    NotifyNewMailArrived n -> assign (asThreadsView . miNewMail) n
    InputValidated err -> do
        allVisible <- gets (concat . visibleViewWidgets)
        case err of
            Just msg | view umContext msg `elem` allVisible -> do
                -- Widget for this message is still visible, display
                -- the message and clear the thread ID.
                assign asUserMessage err
                assign (asAsync . aValidation) Nothing
            _ -> do
                -- Either Nothing (no error), or widget for this message is
                -- not visible.  Clear existing messages and thread states.
                assign asUserMessage Nothing
                assign (asAsync . aValidation) Nothing
appEvent _ = pure ()

initialViews :: Map.Map ViewName View
initialViews = Map.fromList
  [ (Threads, indexView)
  , (ViewMail, mailView)
  , (Help, helpView)
  , (ComposeView, composeView)
  , (FileBrowser, filebrowserView)
  ]

initialState
  :: Configuration
  -> BChan PurebredEvent
  -> Purebred.Storage.Server.Server
  -> (T.Text -> IO ())
  -> HaskelineWidgets
  -> IO AppState
initialState conf chan server sink widgets = do
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
            (view hwSearch widgets)
            (E.editorText ManageMailTagsEditor Nothing "")
            (E.editorText ManageThreadTagsEditor Nothing "")
            0
    mv = MailView
           Nothing
           emptyBodyPresentation
           Filtered
           (L.list MailListOfAttachments mempty 1)
           (E.editorText SaveToDiskPathEditor Nothing "")
           (view hwOpenCommand widgets)
           (E.editorText MailAttachmentPipeToEditor Nothing "")
           (E.editorText ScrollingMailViewFindWordEditor Nothing "")
           0 {- search match index -}
           NoSearch {- MatchInfo -}
    viewsettings =
        ViewSettings
        { _vsViews = initialViews
        , _vsFocusedView = focusRing [Threads, Mails, ViewMail, Help, ComposeView, FileBrowser]
        }
    path = view (confFileBrowserView . fbHomePath) conf
    fb = CreateFileBrowser
         fb'
         (statefulEditor $ E.editor ManageFileBrowserSearchPath Nothing path)
    undostack = UndoStack [] []
    mailboxes = view (confComposeView . cvIdentities) conf
    epoch = UTCTime (fromGregorian 2018 07 18) 1
    async = Async Nothing
    s = AppState conf chan server sink mi mv (initialCompose mailboxes) Nothing viewsettings fb undostack epoch async
  execStateT (runSearch searchterms) s

-- | Application event loop.
theApp ::
     AppState -- ^ initial state
  -> App AppState PurebredEvent Name
theApp s =
    App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = appEvent
    , appStartEvent = pure ()
    , appAttrMap = const (view (asConfig . confTheme) s)
    }
