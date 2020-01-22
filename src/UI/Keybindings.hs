-- This file is part of purebred
-- Copyright (C) 2017-2020 Róman Joost and Fraser Tweedale
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
{-# LANGUAGE RankNTypes #-}

module UI.Keybindings (
  -- * API
  dispatch
  -- * Event Handlers
  -- $eventhandlers
  , nullEventHandler
  , eventHandlerComposeFrom
  , eventHandlerComposeTo
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

import Control.Monad ((<=<))
import qualified Brick.Types as Brick
import qualified Brick.Main as Brick
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Brick.Widgets.Dialog (handleDialogEvent)
import Graphics.Vty (Event (..))
import Control.Lens (Getter, (&), _Left, preview, set, to, view)
import Control.Monad.IO.Class (liftIO)
import Data.Attoparsec.Text (parseOnly)
import Data.List (find)
import Data.Text.Zipper (currentLine)
import Prelude hiding (readFile, unlines)

import Data.RFC5322.Address.Text (mailboxList, addressList)

import Error
import Types
import Purebred.Tags (parseTagOps)
import Purebred.Parsing.Text (niceEndOfInput)
import UI.Validation (dispatchValidation)


-- | Purebreds event handler. Either we can look up a function
-- declared for the key press or send the key press to the Brick widget
-- to handle it.
--
data EventHandler v m = EventHandler
  (forall f. Functor f
    => ([Keybinding v m] -> f [Keybinding v m])
    -> AppState -> f AppState) -- lens to keybindings
  (AppState -> Event -> Brick.EventM Name (Brick.Next AppState)) -- fallback handler

lookupKeybinding :: Event -> [Keybinding v ctx] -> Maybe (Keybinding v ctx)
lookupKeybinding e = find (\x -> view kbEvent x == e)

dispatch :: EventHandler v m -> AppState -> Event -> Brick.EventM Name (Brick.Next AppState)
dispatch (EventHandler l fallback) s ev =
  case lookupKeybinding ev (view l s) of
    Just kb -> s & view (kbAction . aAction) kb . set asError Nothing
    Nothing -> fallback s ev


-- | Simple wrapper around the validation function to not repeating
-- myself pulling the text values out of the lens.
--
runValidation ::
     Monoid a
  => (a -> Maybe Error) -- ^ validation
  -> Getter AppState (E.Editor a n) -- ^ lens to retrieve the text used for validation
  -> AppState
  -> IO AppState
runValidation fx l s =
  dispatchValidation fx asError (view (l . E.editContentsL . to currentLine) s) s

-- $eventhandlers
-- Each event handler is handling a single widget in Purebreds UI

-- | Handlers capable of running used in more than one view
--
composeFromHandler, composeToHandler, manageMailTagHandler ::
     AppState -> Event -> Brick.EventM Name (Brick.Next AppState)

composeFromHandler s e =
  Brick.handleEventLensed s (asCompose . cFrom) E.handleEditorEvent e
  >>= liftIO . runValidation
  (preview (_Left . to GenericError) . parseOnly (mailboxList <* niceEndOfInput)) (asCompose . cFrom)
  >>= Brick.continue

composeToHandler s e =
  Brick.handleEventLensed s (asCompose . cTo) E.handleEditorEvent e
  >>= liftIO . runValidation
  (preview (_Left . to GenericError) . parseOnly (addressList <* niceEndOfInput))
  (asCompose . cTo)
  >>= Brick.continue

manageMailTagHandler s e =
  Brick.handleEventLensed s (asMailIndex . miMailTagsEditor) E.handleEditorEvent e
  >>= liftIO . runValidation (preview _Left . parseTagOps) (asMailIndex . miMailTagsEditor)
  >>= Brick.continue

-- | Do nothing.  It might be worthwhile to enhance this to display
-- a message like "no binding for key <blah>".
--
nullEventHandler :: EventHandler v m
nullEventHandler = EventHandler (\f s -> s <$ f []) (const . Brick.continue)


eventHandlerListOfThreads :: EventHandler 'Threads 'ListOfThreads
eventHandlerListOfThreads = EventHandler
  (asConfig . confIndexView . ivBrowseThreadsKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asMailIndex . miListOfThreads) L.handleListEvent)

eventHandlerSearchThreadsEditor :: EventHandler 'Threads 'SearchThreadsEditor
eventHandlerSearchThreadsEditor = EventHandler
  (asConfig . confIndexView . ivSearchThreadsKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asMailIndex . miSearchThreadsEditor) E.handleEditorEvent)

eventHandlerViewMailManageMailTagsEditor :: EventHandler 'ViewMail 'ManageMailTagsEditor
eventHandlerViewMailManageMailTagsEditor = EventHandler
  (asConfig . confMailView . mvManageMailTagsKeybindings)
  manageMailTagHandler

eventHandlerMailsListOfAttachments:: EventHandler 'ViewMail 'MailListOfAttachments
eventHandlerMailsListOfAttachments = EventHandler
  (asConfig . confMailView . mvMailListOfAttachmentsKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asMailView . mvAttachments) L.handleListEvent)

eventHandlerMailAttachmentOpenWithEditor :: EventHandler 'ViewMail 'MailAttachmentOpenWithEditor
eventHandlerMailAttachmentOpenWithEditor = EventHandler
  (asConfig . confMailView . mvOpenWithKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asMailView . mvOpenCommand) E.handleEditorEvent)

eventHandlerMailAttachmentPipeToEditor :: EventHandler 'ViewMail 'MailAttachmentPipeToEditor
eventHandlerMailAttachmentPipeToEditor = EventHandler
  (asConfig . confMailView . mvPipeToKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asMailView . mvPipeCommand) E.handleEditorEvent)

eventHandlerSaveToDiskEditor :: EventHandler 'ViewMail 'SaveToDiskPathEditor
eventHandlerSaveToDiskEditor = EventHandler
  (asConfig . confMailView . mvSaveToDiskKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asMailView . mvSaveToDiskPath) E.handleEditorEvent)

eventHandlerManageThreadTagsEditor :: EventHandler 'Threads 'ManageThreadTagsEditor
eventHandlerManageThreadTagsEditor =
  EventHandler
    (asConfig . confIndexView . ivManageThreadTagsKeybindings)
    (\s e -> Brick.handleEventLensed s (asMailIndex . miThreadTagsEditor) E.handleEditorEvent e
      >>= liftIO . runValidation (preview _Left . parseTagOps) (asMailIndex . miThreadTagsEditor)
      >>= Brick.continue)

eventHandlerScrollingMailView :: EventHandler 'ViewMail 'ScrollingMailView
eventHandlerScrollingMailView = EventHandler
  (asConfig . confMailView . mvKeybindings)
  (const . Brick.continue)

eventHandlerScrollingMailViewFind :: EventHandler 'ViewMail 'ScrollingMailViewFindWordEditor
eventHandlerScrollingMailViewFind = EventHandler
  (asConfig . confMailView . mvFindWordEditorKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asMailView . mvFindWordEditor) E.handleEditorEvent)

eventHandlerScrollingHelpView :: EventHandler 'Help 'ScrollingHelpView
eventHandlerScrollingHelpView = EventHandler
  (asConfig . confHelpView . hvKeybindings)
  (const . Brick.continue)

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
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asCompose . cSubject) E.handleEditorEvent)

eventHandlerComposeFrom :: EventHandler 'ComposeView 'ComposeFrom
eventHandlerComposeFrom = EventHandler
  (asConfig . confComposeView . cvFromKeybindings)
  composeFromHandler

eventHandlerComposeTo :: EventHandler 'ComposeView 'ComposeTo
eventHandlerComposeTo = EventHandler
  (asConfig . confComposeView . cvToKeybindings)
  composeToHandler

eventHandlerComposeSubject :: EventHandler 'ComposeView 'ComposeSubject
eventHandlerComposeSubject = EventHandler
  (asConfig . confComposeView . cvSubjectKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asCompose . cSubject) E.handleEditorEvent)

eventHandlerConfirm :: EventHandler 'ComposeView 'ConfirmDialog
eventHandlerConfirm = EventHandler
  (asConfig . confComposeView . cvConfirmKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asCompose . cKeepDraft) handleDialogEvent)

eventHandlerComposeListOfAttachments :: EventHandler 'ComposeView 'ComposeListOfAttachments
eventHandlerComposeListOfAttachments = EventHandler
  (asConfig . confComposeView . cvListOfAttachmentsKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asCompose . cAttachments) L.handleListEvent)

eventHandlerComposeFileBrowser :: EventHandler 'FileBrowser 'ListOfFiles
eventHandlerComposeFileBrowser = EventHandler
  (asConfig . confFileBrowserView . fbKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asFileBrowser . fbEntries) L.handleListEvent)

eventHandlerManageFileBrowserSearchPath :: EventHandler 'FileBrowser 'ManageFileBrowserSearchPath
eventHandlerManageFileBrowserSearchPath = EventHandler
  (asConfig . confFileBrowserView . fbSearchPathKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asFileBrowser . fbSearchPath) E.handleEditorEvent)

eventHandlerViewMailComposeTo :: EventHandler 'ViewMail 'ComposeTo
eventHandlerViewMailComposeTo = EventHandler
  (asConfig . confMailView . mvToKeybindings)
  composeToHandler
