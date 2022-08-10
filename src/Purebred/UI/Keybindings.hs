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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Purebred.UI.Keybindings (
  -- * API
  dispatch
  -- * Event Handlers
  -- $eventhandlers
  , nullEventHandler
  , eventHandlerComposeFrom
  , eventHandlerComposeTo
  , eventHandlerComposeCc
  , eventHandlerComposeBcc
  , eventHandlerComposeSubject
  , eventHandlerThreadComposeFrom
  , eventHandlerThreadComposeTo
  , eventHandlerThreadComposeSubject
  , eventHandlerManageThreadTagsEditor
  , eventHandlerMailAttachmentPipeToEditor
  , eventHandlerMailAttachmentOpenWithEditor
  , eventHandlerMailsListOfAttachments
  , eventHandlerListOfThreads
  , eventHandlerViewMailManageMailTagsEditor
  , eventHandlerSearchThreadsEditor
  , eventHandlerComposeListOfAttachments
  , eventHandlerManageFileBrowserSearchPath
  , eventHandlerConfirm
  , eventHandlerScrollingMailView
  , eventHandlerScrollingHelpView
  , eventHandlerComposeFileBrowser
  , eventHandlerScrollingMailViewFind
  , eventHandlerSaveToDiskEditor
  , eventHandlerViewMailComposeTo
  ) where

import qualified Brick.Types as Brick
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.FileBrowser as FB
import Brick.Widgets.Dialog (handleDialogEvent)
import Graphics.Vty (Event (..))
import Control.Lens (Getter, _Left, assign, preview, to, use, view)
import Control.Monad.State
import Data.Attoparsec.Text (parseOnly)
import Data.List (find)
import Data.Text.Zipper (currentLine)
import Data.Text.Zipper.Generic (GenericTextZipper)
import qualified Data.Text as T
import Prelude hiding (readFile, unlines)

import Data.IMF.Text (mailboxList, addressList)

import Purebred.Storage.Tags (parseTagOps)
import Purebred.Types
import Purebred.Types.Parser.Text (niceEndOfInput)
import Purebred.UI.Validation (dispatchValidation)
import Purebred.UI.Widgets (editEditorL)
import Purebred.UI.Notifications (makeWarning)

-- | Purebreds event handler. Either we can look up a function
-- declared for the key press or send the key press to the Brick widget
-- to handle it.
--
data EventHandler v m = EventHandler
  (forall f. Functor f
    => ([Keybinding v m] -> f [Keybinding v m])
    -> AppState -> f AppState) -- lens to keybindings
  (Event -> Brick.EventM Name AppState ()) -- fallback handler

lookupKeybinding :: Event -> [Keybinding v ctx] -> Maybe (Keybinding v ctx)
lookupKeybinding e = find (\x -> view kbEvent x == e)

dispatch :: EventHandler v m -> Event -> Brick.EventM Name AppState ()
dispatch (EventHandler l fallback) ev = do
  kbs <- use l
  case lookupKeybinding ev kbs of
    Just kb -> assign asUserMessage Nothing *> view (kbAction . aAction) kb
    Nothing -> fallback ev

-- | Wrapper for @Brick.Widgets.Edit.handleEditorEvent@ that takes
-- @Graphics.Vty.Event@.
--
-- /brick-0.72/ changed the type of @handleEditorEvent@ to take a
-- 'BrickEvent' rather than vty 'Event'.  We implemented this wrapper
-- rather than the more invasive approach of changing the event type
-- we pass around in our event handling framework.
--
-- We can make the bigger change in the future, e.g. if we want our
-- widgts to handle mouse events.
--
handleEditorVtyEvent
  :: (Eq n, E.DecodeUtf8 t, Eq t, GenericTextZipper t)
  => Graphics.Vty.Event -> Brick.EventM n (E.Editor t n) ()
handleEditorVtyEvent = E.handleEditorEvent . Brick.VtyEvent


-- | Simple wrapper around the validation function to not repeating
-- myself pulling the text values out of the lens.
--
runValidation
  :: (Monoid a, MonadIO m, MonadState AppState m)
  => (a -> Maybe UserMessage) -- ^ validation
  -> Getter AppState (E.Editor a n) -- ^ lens to retrieve the text used for validation
  -> m ()
runValidation fx l = do
  v <- use (l . E.editContentsL . to currentLine)
  dispatchValidation fx v

-- $eventhandlers
-- Each event handler is handling a single widget in Purebreds UI

-- | Handlers capable of running used in more than one view
--
composeFromHandler, composeToHandler, composeCcHandler, composeBccHandler, manageMailTagHandler
  :: Event -> Brick.EventM Name AppState ()

composeFromHandler e = do
  Brick.zoom (asCompose . cFrom . editEditorL) (handleEditorVtyEvent e)
  runValidation
    (preview (_Left . to (makeWarning ComposeFrom . T.pack)) . parseOnly (mailboxList <* niceEndOfInput))
    (asCompose . cFrom . editEditorL)

composeToHandler e = do
  Brick.zoom (asCompose . cTo . editEditorL) (handleEditorVtyEvent e)
  runValidation
    (preview (_Left . to (makeWarning ComposeTo . T.pack)) . parseOnly (addressList <* niceEndOfInput))
    (asCompose . cTo . editEditorL)

composeCcHandler e = do
  Brick.zoom (asCompose . cCc . editEditorL) (handleEditorVtyEvent e)
  runValidation
    (preview (_Left . to (makeWarning ComposeCc . T.pack)) . parseOnly (addressList <* niceEndOfInput))
    (asCompose . cCc . editEditorL)

composeBccHandler e = do
  Brick.zoom (asCompose . cBcc . editEditorL) (handleEditorVtyEvent e)
  runValidation
    (preview (_Left . to (makeWarning ComposeBcc . T.pack)) . parseOnly (addressList <* niceEndOfInput))
    (asCompose . cBcc . editEditorL)

manageMailTagHandler e = do
  Brick.zoom (asThreadsView . miMailTagsEditor) (handleEditorVtyEvent e)
  runValidation (preview _Left . parseTagOps) (asThreadsView . miMailTagsEditor)

-- | Do nothing.  It might be worthwhile to enhance this to display
-- a message like "no binding for key <blah>".
--
nullEventHandler :: EventHandler v m
nullEventHandler = EventHandler (\f s -> s <$ f []) (\_e -> pure ())


eventHandlerListOfThreads :: EventHandler 'Threads 'ListOfThreads
eventHandlerListOfThreads = EventHandler
  (asConfig . confIndexView . ivBrowseThreadsKeybindings)
  (Brick.zoom (asThreadsView . miListOfThreads) . L.handleListEvent)

eventHandlerSearchThreadsEditor :: EventHandler 'Threads 'SearchThreadsEditor
eventHandlerSearchThreadsEditor = EventHandler
  (asConfig . confIndexView . ivSearchThreadsKeybindings)
  (Brick.zoom (asThreadsView . miSearchThreadsEditor . editEditorL) . handleEditorVtyEvent)

eventHandlerViewMailManageMailTagsEditor :: EventHandler 'ViewMail 'ManageMailTagsEditor
eventHandlerViewMailManageMailTagsEditor = EventHandler
  (asConfig . confMailView . mvManageMailTagsKeybindings)
  manageMailTagHandler

eventHandlerMailsListOfAttachments:: EventHandler 'ViewMail 'MailListOfAttachments
eventHandlerMailsListOfAttachments = EventHandler
  (asConfig . confMailView . mvMailListOfAttachmentsKeybindings)
  (Brick.zoom (asMailView . mvAttachments) . L.handleListEvent)

eventHandlerMailAttachmentOpenWithEditor :: EventHandler 'ViewMail 'MailAttachmentOpenWithEditor
eventHandlerMailAttachmentOpenWithEditor = EventHandler
  (asConfig . confMailView . mvOpenWithKeybindings)
  (Brick.zoom (asMailView . mvOpenCommand) . handleEditorVtyEvent)

eventHandlerMailAttachmentPipeToEditor :: EventHandler 'ViewMail 'MailAttachmentPipeToEditor
eventHandlerMailAttachmentPipeToEditor = EventHandler
  (asConfig . confMailView . mvPipeToKeybindings)
  (Brick.zoom (asMailView . mvPipeCommand) . handleEditorVtyEvent)

eventHandlerSaveToDiskEditor :: EventHandler 'ViewMail 'SaveToDiskPathEditor
eventHandlerSaveToDiskEditor = EventHandler
  (asConfig . confMailView . mvSaveToDiskKeybindings)
  (Brick.zoom (asMailView . mvSaveToDiskPath) . handleEditorVtyEvent)

eventHandlerManageThreadTagsEditor :: EventHandler 'Threads 'ManageThreadTagsEditor
eventHandlerManageThreadTagsEditor =
  EventHandler
    (asConfig . confIndexView . ivManageThreadTagsKeybindings)
    (\e -> do
      Brick.zoom (asThreadsView . miThreadTagsEditor) (handleEditorVtyEvent e)
      runValidation
        (preview _Left . parseTagOps)
        (asThreadsView . miThreadTagsEditor)
    )

eventHandlerScrollingMailView :: EventHandler 'ViewMail 'ScrollingMailView
eventHandlerScrollingMailView = EventHandler
  (asConfig . confMailView . mvKeybindings)
  (\_e -> pure ())

eventHandlerScrollingMailViewFind :: EventHandler 'ViewMail 'ScrollingMailViewFindWordEditor
eventHandlerScrollingMailViewFind = EventHandler
  (asConfig . confMailView . mvFindWordEditorKeybindings)
  (Brick.zoom (asMailView . mvFindWordEditor) . handleEditorVtyEvent)

eventHandlerScrollingHelpView :: EventHandler 'Help 'ScrollingHelpView
eventHandlerScrollingHelpView = EventHandler
  (asConfig . confHelpView . hvKeybindings)
  (\_e -> pure ())

eventHandlerThreadComposeFrom :: EventHandler 'Threads 'ComposeFrom
eventHandlerThreadComposeFrom = EventHandler
  (asConfig . confIndexView . ivFromKeybindings)
  composeFromHandler


eventHandlerThreadComposeTo :: EventHandler 'Threads 'ComposeTo
eventHandlerThreadComposeTo = EventHandler
  (asConfig . confIndexView . ivToKeybindings)
  composeToHandler

eventHandlerThreadComposeSubject :: EventHandler 'Threads 'ComposeSubject
eventHandlerThreadComposeSubject = EventHandler
  (asConfig . confIndexView . ivSubjectKeybindings)
  (Brick.zoom (asCompose . cSubject . editEditorL) . handleEditorVtyEvent)

eventHandlerComposeFrom :: EventHandler 'ComposeView 'ComposeFrom
eventHandlerComposeFrom = EventHandler
  (asConfig . confComposeView . cvFromKeybindings)
  composeFromHandler

eventHandlerComposeTo :: EventHandler 'ComposeView 'ComposeTo
eventHandlerComposeTo = EventHandler
  (asConfig . confComposeView . cvToKeybindings)
  composeToHandler

eventHandlerComposeCc :: EventHandler 'ComposeView 'ComposeCc
eventHandlerComposeCc = EventHandler
  (asConfig . confComposeView . cvCcKeybindings)
  composeCcHandler

eventHandlerComposeBcc :: EventHandler 'ComposeView 'ComposeBcc
eventHandlerComposeBcc = EventHandler
  (asConfig . confComposeView . cvBccKeybindings)
  composeBccHandler

eventHandlerComposeSubject :: EventHandler 'ComposeView 'ComposeSubject
eventHandlerComposeSubject = EventHandler
  (asConfig . confComposeView . cvSubjectKeybindings)
  (Brick.zoom (asCompose . cSubject . editEditorL) . handleEditorVtyEvent)

eventHandlerConfirm :: EventHandler 'ComposeView 'ConfirmDialog
eventHandlerConfirm = EventHandler
  (asConfig . confComposeView . cvConfirmKeybindings)
  (Brick.zoom (asCompose . cKeepDraft) . handleDialogEvent)

eventHandlerComposeListOfAttachments :: EventHandler 'ComposeView 'ComposeListOfAttachments
eventHandlerComposeListOfAttachments = EventHandler
  (asConfig . confComposeView . cvListOfAttachmentsKeybindings)
  (Brick.zoom (asCompose . cAttachments) . L.handleListEvent)

eventHandlerComposeFileBrowser :: EventHandler 'FileBrowser 'ListOfFiles
eventHandlerComposeFileBrowser = EventHandler
  (asConfig . confFileBrowserView . fbKeybindings)
  (Brick.zoom (asFileBrowser . fbEntries) . FB.handleFileBrowserEvent)

eventHandlerManageFileBrowserSearchPath :: EventHandler 'FileBrowser 'ManageFileBrowserSearchPath
eventHandlerManageFileBrowserSearchPath = EventHandler
  (asConfig . confFileBrowserView . fbSearchPathKeybindings)
  (Brick.zoom (asFileBrowser . fbSearchPath . editEditorL) . handleEditorVtyEvent)

eventHandlerViewMailComposeTo :: EventHandler 'ViewMail 'ComposeTo
eventHandlerViewMailComposeTo = EventHandler
  (asConfig . confMailView . mvToKeybindings)
  composeToHandler
