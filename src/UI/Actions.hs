-- This file is part of purebred
-- Copyright (C) 2017-2019 Róman Joost and Fraser Tweedale
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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module UI.Actions (
  -- * Overview
  -- $overview

  -- * Examples
  -- $examples

  -- * Adding new Actions
  -- $new_actions

  Scrollable(..)
  , Completable(..)
  , HasEditor(..)
  , HasName(..)
  -- * Actions

  -- ** Brick Event Loop Actions
  -- $brick_actions
  , quit
  , continue
  , edit
  , invokeEditor
  , openWithCommand
  , pipeToCommand

  -- ** Keybinding Actions
  -- $keybinding_actions
  , focus
  , done
  , abort
  , noop
  , chain
  , chain'

  -- ** List specific Actions
  , listUp
  , listDown
  , listJumpToEnd
  , listJumpToStart
  , reloadList
  , toggleListItem

  -- ** Mail specific Actions
  , displayMail
  , setUnread
  , displayThreadMails
  , toggleHeaders
  , switchComposeEditor
  , replyMail
  , selectNextUnread
  , composeAsNew
  , createAttachments
  , openAttachment
  , setTags

  -- ** Actions for scrolling
  , scrollUp
  , scrollDown
  , scrollPageUp
  , scrollPageDown
  , scrollNextWord
  , removeHighlights

  -- ** Actions for composing mails
  , delete

  -- ** Actions only used for a specific widget context
  , enterDirectory
  , parentDirectory
  , handleConfirm

  -- * API
  , applySearch
  , initialCompose
  ) where

import Data.Functor.Identity (Identity(..))

import qualified Brick
import Brick.BChan (writeBChan)
import qualified Brick.Focus as Brick
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Brick.Widgets.Dialog (dialog, dialogSelection, Dialog)
import Network.Mime (defaultMimeLookup)
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LB
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.Vector.Lens (vector)
import Data.List.NonEmpty (NonEmpty(..))
import System.FilePath (takeDirectory, (</>))
import qualified Data.Vector as Vector
import Prelude hiding (readFile, unlines)
import Data.Functor (($>))
import Control.Lens
       (_Just, to, at, ix, _1, _2, toListOf, traverse, traversed, has, snoc,
        filtered, set, over, preview, view, views, (&), nullOf, firstOf, non,
        Getting, Lens', folded)
import Control.Concurrent (forkIO)
import Control.Monad ((>=>))
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (runExceptT, MonadError, throwError)
import Control.Exception (catch, IOException)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text.Zipper
       (insertMany, currentLine, gotoEOL, clearZipper, TextZipper)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getZonedTime, zonedTimeToUTC)

import qualified Data.RFC5322.Address.Text as AddressText (renderMailboxes)
import Data.MIME
       (createMultipartMixedMessage, contentTypeApplicationOctetStream,
        createTextPlainMessage, createAttachmentFromFile, renderMessage,
        contentDisposition, dispositionType, headers, filename,
        parseContentType, attachments, entities, matchContentType,
        contentType, mailboxList, renderMailboxes, addressList, renderAddresses,
        renderRFC5422Date, MIMEMessage, WireEntity, DispositionType(..),
        ContentType(..), Mailbox(..),
        CharsetLookup)
import qualified Storage.Notmuch as Notmuch
import Storage.ParsedMail
       ( parseMail, getTo, getFrom, getSubject, toQuotedMail
       , entityToBytes, toMIMEMessage, takeFileName, bodyToDisplay
       , removeMatchingWords, findMatchingWords, makeScrollSteps)
import Types
import Error
import UI.Utils (selectedFiles)
import UI.Views
       (mailView, toggleLastVisibleWidget, indexView, resetView,
        focusedViewWidget)
import Purebred.Events (nextGeneration)
import Purebred.LazyVector (V)
import Purebred.Tags (parseTagOps)
import Purebred.System.Directory (listDirectory')
import Purebred.System.Process

{- $overview

Actions are composible functions. They can be sequenced and used in 'Keybinding's.

-}

{- $examples

This keybinding registered to back space scrolls a page up and
continues with the event loop:

@
'Keybinding' (EvKey KBS []) ('scrollPageUp' ``chain'`` 'continue') ]
@

This keybinding is used to change the focus to a different widget:

@
'Keybinding' (EvKey (KChar 'q') []) ('noop' ``chain'`` ('focus' :: Action 'Threads 'ListOfThreads 'AppState') ``chain`` 'continue'
@
-}

{- $new_actions
New 'Action's are typically added when creating a 'Keybinding'
first. A simple Action to start off has a specific 'View' and widget
(see 'Name'). You can access the full 'AppState' in the Action's
function including 'IO'.
-}

-- | Scrollable is an abstraction over Brick's viewport scroller in
-- order to support viewing larger amounts of text.
--
class Scrollable (n :: Name) where
  makeViewportScroller :: Proxy n -> Brick.ViewportScroll Name

instance Scrollable 'ScrollingMailView where
  makeViewportScroller _ = Brick.viewportScroll ScrollingMailView

instance Scrollable 'ScrollingHelpView where
  makeViewportScroller _ = Brick.viewportScroll ScrollingHelpView

-- | Abstraction to access editors (via it's lens) in the current
-- context with a proxy type.
--
class HasEditor (n :: Name) where
  editorL :: Proxy n -> Lens' AppState (E.Editor T.Text Name)

instance HasEditor 'ComposeFrom where
  editorL _ = asCompose . cFrom

instance HasEditor 'ComposeTo where
  editorL _ = asCompose . cTo

instance HasEditor 'ComposeSubject where
  editorL _ = asCompose . cSubject

instance HasEditor 'ManageMailTagsEditor where
  editorL _ = asMailIndex . miMailTagsEditor

instance HasEditor 'MailAttachmentOpenWithEditor where
  editorL _ = asMailView . mvOpenCommand

instance HasEditor 'MailAttachmentPipeToEditor where
  editorL _ = asMailView . mvPipeCommand

instance HasEditor 'ScrollingMailViewFindWordEditor where
  editorL _ = asMailView . mvFindWordEditor

instance HasEditor 'SearchThreadsEditor where
  editorL _ = asMailIndex . miSearchThreadsEditor

instance HasEditor 'ManageThreadTagsEditor where
  editorL _ = asMailIndex . miThreadTagsEditor

-- | Contexts that have a navigable (Brick) list
class HasList (n :: Name) where
  type T n :: * -> *
  type E n
  list :: Proxy n -> Lens' AppState (L.GenericList Name (T n) (E n))

instance HasList 'ListOfThreads where
  type T 'ListOfThreads = V
  type E 'ListOfThreads = NotmuchThread
  list _ = asMailIndex . miListOfThreads

instance HasList 'ListOfMails where
  type T 'ListOfMails = Vector.Vector
  type E 'ListOfMails = NotmuchMail
  list _ = asMailIndex . miListOfMails

instance HasList 'ScrollingMailView where
  type T 'ScrollingMailView = Vector.Vector
  type E 'ScrollingMailView = NotmuchMail
  list _ = asMailIndex . miListOfMails

instance HasList 'ComposeListOfAttachments where
  type T 'ComposeListOfAttachments = Vector.Vector
  type E 'ComposeListOfAttachments = MIMEMessage
  list _ = asCompose . cAttachments

instance HasList 'MailListOfAttachments where
  type T 'MailListOfAttachments = Vector.Vector
  type E 'MailListOfAttachments = WireEntity
  list _ = asMailView . mvAttachments

instance HasList 'ListOfFiles where
  type T 'ListOfFiles = Vector.Vector
  type E 'ListOfFiles = (Bool, FileSystemEntry)
  list _ = asFileBrowser . fbEntries


-- | A function which is run at the end of a chained sequence of actions.
--
-- For example: the user changes the notmuch search terms to find a
-- particular mail. To apply his changes, he 'completes' his typed in
-- text by pressing Enter.
--
class Completable (m :: Name) where
  complete :: Proxy m -> AppState -> T.EventM Name AppState

instance Completable 'SearchThreadsEditor where
  complete _ = applySearch

instance Completable 'ManageMailTagsEditor where
  complete _ s = liftIO $ over (asMailIndex . miMailTagsEditor . E.editContentsL) clearZipper <$> completeMailTags s

instance Completable 'ComposeListOfAttachments where
  complete _ = sendMail

-- | Apply all given tag operations to existing mails
--
completeMailTags :: AppState -> IO AppState
completeMailTags s =
    case getEditorTagOps (Proxy :: Proxy 'ManageMailTagsEditor) s of
        Left err -> pure $ setError err s
        Right ops' -> over (asMailIndex . miListOfThreads) (L.listModify (Notmuch.tagItem ops'))
                      <$> selectedItemHelper (asMailIndex . miListOfMails) s (manageMailTags s ops')

instance Completable 'ComposeTo where
  complete _ = pure . set (asViews . vsViews . at ComposeView . _Just
                           . vLayers . ix 1 . ix ComposeTo . veState) Hidden

instance Completable 'ComposeFrom where
  complete _ = pure . set (asViews . vsViews . at ComposeView . _Just
                           . vLayers . ix 1 . ix ComposeFrom . veState) Hidden

instance Completable 'ComposeSubject where
  complete _ = pure . set (asViews . vsViews . at ComposeView . _Just
                           . vLayers . ix 1 . ix ComposeSubject . veState) Hidden

instance Completable 'ConfirmDialog where
  complete _ = pure . set (asViews . vsViews . at ComposeView . _Just . vLayers . ix 0 . ix ConfirmDialog . veState) Hidden

-- | Applying tag operations on threads
-- Note: notmuch does not support adding tags to threads themselves, instead we'll
-- apply all tag operations on mails in the thread. Instead of reloading the
-- thread, we'll apply all tag operations on the thread type as well, which are
-- not persisted to the database. This strategy is faster since it does not need
-- any database access above tagging mails, but it could pose a problem if tags
-- don't show up in the UI.
--
instance Completable 'ManageThreadTagsEditor where
  complete _ s = case getEditorTagOps (Proxy :: Proxy 'ManageThreadTagsEditor) s of
                      Left err -> pure $ setError err s
                      Right ops ->
                        toggleLastVisibleWidget SearchThreadsEditor
                        <$> selectedItemHelper (asMailIndex . miListOfThreads) s (manageThreadTags s ops)

instance Completable 'ManageFileBrowserSearchPath where
  complete _ s =
    ($ s)
    <$> (either setError updateBrowseFileContents
         <$> runExceptT (listDirectory' (currentLine $ view (asFileBrowser . fbSearchPath . E.editContentsL) s)))

instance Completable 'MailAttachmentOpenWithEditor where
  complete _ = pure
               . set (asViews . vsViews . at ViewMail . _Just . vLayers . ix 0
                      . ix MailAttachmentOpenWithEditor . veState) Hidden

instance Completable 'MailAttachmentPipeToEditor where
  complete _ = pure
               . set (asViews . vsViews . at ViewMail . _Just . vLayers . ix 0
                      . ix MailAttachmentPipeToEditor . veState) Hidden

instance Completable 'ScrollingMailViewFindWordEditor where
  complete _ s =
    let needle = view (asMailView . mvFindWordEditor . E.editContentsL . to currentLine) s
        mbody = findMatchingWords needle $ view (asMailView . mvBody) s
    in pure $ s &
       set (asViews . vsViews . at ViewMail . _Just . vLayers . ix 0
                      . ix ScrollingMailViewFindWordEditor . veState) Hidden
       . set (asMailView . mvScrollSteps) (Brick.focusRing (makeScrollSteps mbody))
       . set (asMailView . mvBody) mbody

-- | Generalisation of reset actions, whether they reset editors back to their
-- initial state or throw away composed, but not yet sent mails.
--
class Resetable (v :: ViewName) (m :: Name) where
  reset :: Proxy v -> Proxy m -> AppState -> T.EventM Name AppState

instance Resetable 'Threads 'SearchThreadsEditor where
  reset _ _ = pure

instance Resetable 'ViewMail 'ManageMailTagsEditor where
  reset _ _ = pure . over (asMailIndex . miMailTagsEditor . E.editContentsL) clearZipper

instance Resetable 'Threads 'ManageThreadTagsEditor where
  reset _ _ s = pure $ s
              & over (asMailIndex . miThreadTagsEditor . E.editContentsL) clearZipper
              . toggleLastVisibleWidget SearchThreadsEditor

instance Resetable 'Threads 'ComposeFrom where
  reset _ _ = pure . clearMailComposition

instance Resetable 'Threads 'ComposeSubject where
  reset _ _ = pure . clearMailComposition

instance Resetable 'Threads 'ComposeTo where
  reset _ _ = pure . clearMailComposition

instance Resetable 'ComposeView 'ComposeFrom where
  reset _ _ s = pure $ s & over (asCompose . cSubject . E.editContentsL) (revertEditorContents s)
                . set (asViews . vsViews . at ComposeView . _Just . vLayers . ix 1
                       . ix ComposeFrom . veState) Hidden

instance Resetable 'ComposeView 'ComposeTo where
  reset _ _ s = pure $ s & over (asCompose . cTo . E.editContentsL) (revertEditorContents s)
                . set (asViews . vsViews . at ComposeView . _Just . vLayers . ix 1
                       . ix ComposeTo . veState) Hidden

instance Resetable 'ComposeView 'ComposeSubject where
  reset _ _ s = pure $ s & over (asCompose . cSubject . E.editContentsL) (revertEditorContents s)
                . set (asViews . vsViews . at ComposeView . _Just . vLayers . ix 1
                       . ix ComposeTo . veState) Hidden

revertEditorContents :: AppState -> TextZipper T.Text -> TextZipper T.Text
revertEditorContents s z = let saved = view (asCompose . cTemp) s
                               replace = insertMany saved . clearZipper
                           in replace z

instance Resetable 'ComposeView 'ComposeListOfAttachments where
  reset _ _ = pure . clearMailComposition

instance Resetable 'FileBrowser 'ManageFileBrowserSearchPath where
  reset _ _ = pure . over (asFileBrowser . fbSearchPath . E.editContentsL) clearZipper

instance Resetable 'ViewMail 'MailListOfAttachments where
  reset _ _ = pure . set (asViews . vsViews . at ViewMail . _Just . vLayers . ix 0
                          . ix MailListOfAttachments . veState) Hidden

instance Resetable 'ViewMail 'MailAttachmentOpenWithEditor where
  reset _ _ = pure . over (asMailView . mvOpenCommand . E.editContentsL) clearZipper
            . set (asViews . vsViews . at ViewMail . _Just . vLayers . ix 0
                   . ix MailAttachmentOpenWithEditor . veState) Hidden

instance Resetable 'ViewMail 'MailAttachmentPipeToEditor where
  reset _ _ = pure . over (asMailView . mvPipeCommand . E.editContentsL) clearZipper
            . set (asViews . vsViews . at ViewMail . _Just . vLayers . ix 0
                   . ix MailAttachmentPipeToEditor . veState) Hidden

instance Resetable 'ViewMail 'ScrollingMailViewFindWordEditor where
  reset _ _ = pure . over (asMailView . mvFindWordEditor . E.editContentsL) clearZipper
              . set (asViews . vsViews . at ViewMail . _Just . vLayers . ix 0
                    . ix ScrollingMailViewFindWordEditor . veState) Hidden
              . resetMatchingWords

instance Resetable 'ViewMail 'ScrollingMailView where
  reset _ _ = pure . resetMatchingWords

-- | Reset the composition state for a new mail
--
clearMailComposition :: AppState -> AppState
clearMailComposition s =
    let mailboxes = view (asConfig . confComposeView . cvIdentities) s
    in s
        -- insert default from addresses
        & set asCompose (initialCompose mailboxes)
        -- reset the UI
        -- Note: Only replace the last widget on the Threads view with the
        -- SearchThreadsEditor. This is important if we abort mail composition
        -- when we're still looking at a list of threads. The composition can
        -- also be aborted in the composition editor, with which we would not
        -- want to replace anything. Implement #181 to fix this.
        . toggleLastVisibleWidget SearchThreadsEditor

-- | Generalisation of focus changes between widgets on the same
-- "view" expressed with the mode in the application state.
--
class Focusable (v :: ViewName) (m :: Name) where
  switchFocus :: Proxy v -> Proxy m -> AppState -> T.EventM Name AppState

instance Focusable 'Threads 'SearchThreadsEditor where
  switchFocus _ _ = pure . over (asMailIndex . miSearchThreadsEditor) (E.applyEdit gotoEOL)

instance Focusable 'Threads 'ManageThreadTagsEditor where
  switchFocus _ _ s = pure $ s &
                    over (asMailIndex . miThreadTagsEditor . E.editContentsL) clearZipper
                    . toggleLastVisibleWidget ManageThreadTagsEditor

instance Focusable 'Threads 'ComposeFrom where
  switchFocus = focusComposeFrom

instance Focusable 'Threads 'ComposeTo where
  switchFocus _ _ = pure . toggleLastVisibleWidget ComposeTo

instance Focusable 'Threads 'ComposeSubject where
  switchFocus _ _ = pure . toggleLastVisibleWidget ComposeSubject

instance Focusable 'Threads 'ListOfThreads where
  switchFocus _ _ = pure

instance Focusable 'ViewMail 'ManageMailTagsEditor where
  switchFocus _ _ s = pure $ s &
                      set (asViews . vsViews . at ViewMail . _Just . vLayers . ix 0
                           . ix ManageMailTagsEditor . veState) Visible
                      . set (asViews . vsViews . at ViewMail . _Just . vFocus) ManageMailTagsEditor

instance Focusable 'ViewMail 'ScrollingMailView where
  switchFocus _ _ = pure . set (asViews. vsViews . at ViewMail . _Just . vFocus) ScrollingMailView

instance Focusable 'ViewMail 'ScrollingMailViewFindWordEditor where
  switchFocus _ _ = pure
                    . over (asMailView . mvFindWordEditor . E.editContentsL) clearZipper
                    . set (asViews. vsViews . at ViewMail . _Just . vFocus) ScrollingMailViewFindWordEditor
                    . set (asViews . vsViews . at ViewMail . _Just . vLayers . ix 0
                           . ix ScrollingMailViewFindWordEditor . veState) Visible

instance Focusable 'ViewMail 'ListOfMails where
  switchFocus _ _ = pure . set (asViews . vsViews . at ViewMail . _Just . vFocus) ListOfMails

instance Focusable 'ViewMail 'MailListOfAttachments where
  switchFocus _ _ = pure . set (asViews . vsViews . at ViewMail . _Just . vFocus) MailListOfAttachments
                    . set (asViews . vsViews . at ViewMail . _Just . vLayers . ix 0
                           . ix MailListOfAttachments . veState) Visible

instance Focusable 'ViewMail 'MailAttachmentOpenWithEditor where
  switchFocus _ _ = pure . set (asViews . vsViews . at ViewMail . _Just . vFocus) MailAttachmentOpenWithEditor
                    . set (asViews . vsViews . at ViewMail . _Just . vLayers . ix 0
                           . ix MailAttachmentOpenWithEditor . veState) Visible

instance Focusable 'ViewMail 'MailAttachmentPipeToEditor where
  switchFocus _ _ = pure . set (asViews . vsViews . at ViewMail . _Just . vFocus) MailAttachmentPipeToEditor
                    . set (asViews . vsViews . at ViewMail . _Just . vLayers . ix 0
                           . ix MailAttachmentPipeToEditor . veState) Visible

instance Focusable 'Help 'ScrollingHelpView where
  switchFocus _ _ = pure . over (asViews . vsFocusedView) (Brick.focusSetCurrent Help)

instance Focusable 'ComposeView 'ComposeListOfAttachments where
  switchFocus _ _ s = pure $ s & set (asViews . vsViews . at ComposeView . _Just . vFocus) ComposeListOfAttachments
                    . resetView Threads indexView

instance Focusable 'ComposeView 'ComposeFrom where
  switchFocus _ _ s = pure $ s & set (asViews . vsViews . at ComposeView . _Just . vFocus) ComposeFrom
                      . set (asCompose . cTemp) (view (asCompose . cTo . E.editContentsL . to currentLine) s)
                      . set (asViews . vsViews . at ComposeView . _Just . vLayers . ix 1
                             . ix ComposeFrom . veState) Visible

instance Focusable 'ComposeView 'ComposeTo where
  switchFocus _ _ s = pure $ s & set (asViews . vsViews . at ComposeView . _Just . vFocus) ComposeTo
                      . set (asCompose . cTemp) (view (asCompose . cTo . E.editContentsL . to currentLine) s)
                      . set (asViews . vsViews . at ComposeView . _Just . vLayers . ix 1
                             . ix ComposeTo . veState) Visible

instance Focusable 'ComposeView 'ComposeSubject where
  switchFocus _ _ s = pure $ s & set (asViews . vsViews . at ComposeView . _Just . vFocus) ComposeSubject
                      . set (asCompose . cTemp) (view (asCompose . cTo . E.editContentsL . to currentLine) s)
                      . set (asViews . vsViews . at ComposeView . _Just . vLayers . ix 1
                             . ix ComposeSubject . veState) Visible

instance Focusable 'ComposeView 'ConfirmDialog where
  switchFocus _ _ s = pure $ s & set (asViews . vsViews . at ComposeView . _Just . vFocus) ConfirmDialog
                      . set (asViews . vsViews . at ComposeView . _Just . vLayers . ix 0
                             . ix ConfirmDialog . veState) Visible

instance Focusable 'FileBrowser 'ListOfFiles where
  switchFocus _ _ s = let path = view (asFileBrowser . fbSearchPath . E.editContentsL . to currentLine) s
                      in ($ s)
                         <$> (either setError (\x -> over (asFileBrowser . fbSearchPath . E.editContentsL)
                                                     (insertMany path . clearZipper)
                                                     . updateBrowseFileContents x)
                              <$> runExceptT (listDirectory' path))

instance Focusable 'FileBrowser 'ManageFileBrowserSearchPath where
  switchFocus _ _ = pure


-- In case the user is already composing a new mail, go back to the compose
-- editor, otherwise focus the editor to input the from address.
focusComposeFrom
    :: Applicative f
    => proxy1
    -> proxy2
    -> AppState
    -> f AppState
focusComposeFrom _ _ s =
    if nullOf (asCompose . cAttachments) s
        then pure $
             over
                 (asViews . vsFocusedView)
                 (Brick.focusSetCurrent ComposeView)
                 s
        else pure $ s & toggleLastVisibleWidget ComposeFrom
             . over (asCompose . cFrom) (E.applyEdit gotoEOL)

-- | Generalisation in order to access the widget name from a phantom
-- type
--
class HasName (a :: Name) where
  name :: Proxy a -> Name

instance HasName 'ListOfMails where
  name _ = ListOfMails

instance HasName 'SearchThreadsEditor where
  name _ = SearchThreadsEditor

instance HasName 'ScrollingMailView where
  name _ = ScrollingMailView

instance HasName 'ScrollingMailViewFindWordEditor where
  name _ = ScrollingMailViewFindWordEditor

instance HasName 'ManageMailTagsEditor where
  name _ = ManageMailTagsEditor

instance HasName 'ListOfThreads where
  name _ = ListOfThreads

instance HasName 'ScrollingHelpView where
  name _ = ScrollingHelpView

instance HasName 'ComposeFrom where
  name _ = ComposeFrom

instance HasName 'ComposeTo where
  name _ = ComposeTo

instance HasName 'ComposeSubject where
  name _ = ComposeSubject

instance HasName 'ManageThreadTagsEditor where
  name _ = ManageThreadTagsEditor

instance HasName 'ComposeListOfAttachments where
  name _ = ComposeListOfAttachments

instance HasName 'ListOfFiles where
  name _ = ListOfFiles

instance HasName 'ManageFileBrowserSearchPath where
  name _ = ManageFileBrowserSearchPath

instance HasName 'MailListOfAttachments where
  name _ = MailListOfAttachments

instance HasName 'MailAttachmentOpenWithEditor where
  name _ = MailAttachmentOpenWithEditor

instance HasName 'MailAttachmentPipeToEditor where
  name _ = MailAttachmentPipeToEditor

instance HasName 'ConfirmDialog where
  name _ = ConfirmDialog

-- | Allow to switch from the current active view to a different
-- view. Instances are view transitions we only permit.
--
class ViewTransition (v :: ViewName) (v' :: ViewName) where
  transitionHook :: Proxy v -> Proxy v' -> AppState -> AppState
  transitionHook _ _ = id

instance ViewTransition v v where

instance ViewTransition 'Threads 'ComposeView where

instance ViewTransition 'Help v where

instance ViewTransition v 'Help where

instance ViewTransition 'ComposeView 'Threads where

instance ViewTransition 'ComposeView 'FileBrowser where

instance ViewTransition 'Threads 'ViewMail where
  transitionHook _ _ = set (asViews . vsViews . ix ViewMail) mailView

instance ViewTransition 'ViewMail 'ComposeView where

instance ViewTransition 'FileBrowser 'ComposeView where

instance ViewTransition 'ViewMail 'Threads where


-- | Generalisation to access the view name from a phantom type. This
-- is useful when switching views.
--
class HasViewName (a :: ViewName) where
  viewname :: Proxy a -> ViewName

instance HasViewName 'Threads where
  viewname _ = Threads

instance HasViewName 'ViewMail where
  viewname _ = ViewMail

instance HasViewName 'Help where
  viewname _ = Help

instance HasViewName 'ComposeView where
  viewname _ = ComposeView

instance HasViewName 'FileBrowser where
  viewname _ = FileBrowser


-- $brick_actions
-- These actions wrap Brick's event loop functions. They are used to
-- indicate whether we continue the event loop or halt (quit). These
-- actions typically finish a sequence of chained Actions. Use them at
-- the __end__ of a chained sequence.
--
-- Note:
-- While only 'quit' and 'continue' falls into this category, more
-- Purebred functions fall into this category because we're missing
-- ways to modularise them. See #294
--
quit :: Action v ctx (T.Next AppState)
quit = Action ["quit the application"] Brick.halt

-- | A noop used to continue the Brick event loop.
--
continue :: Action v ctx (T.Next AppState)
continue = Action mempty Brick.continue

-- | Suspends Purebred and invokes the configured editor.
--
invokeEditor :: Action v ctx (T.Next AppState)
invokeEditor = Action ["invoke external editor"] (Brick.suspendAndResume . liftIO . invokeEditor')

-- | Suspends Purebred to invoke a command for editing an
-- attachment. Currently only supports re-editing the body text of an
-- e-mail.
--
edit :: Action 'ComposeView 'ComposeListOfAttachments (T.Next AppState)
edit = Action ["edit file"] (Brick.suspendAndResume . liftIO . editAttachment)

openAttachment :: Action 'ViewMail ctx (T.Next AppState)
openAttachment =
  Action
  { _aDescription = ["open attachment with external command"]
  , _aAction = \s ->
      let
        match ct = firstOf (asConfig . confMailView . mvMailcap . traversed
                            . filtered (`fst` ct)
                            . _2) s
        maybeCommand =
          match
          =<< preview (asMailView . mvAttachments . to L.listSelectedElement . _Just . _2 . headers . contentType) s
      in case maybeCommand of
           (Just cmd) -> Brick.suspendAndResume $ liftIO $ openCommand' s cmd
           Nothing ->
             Brick.continue
             $ s & set (asViews . vsViews . at ViewMail . _Just . vFocus) MailAttachmentOpenWithEditor
             . set (asViews . vsViews . at ViewMail . _Just . vLayers . ix 0
                    . ix MailAttachmentOpenWithEditor . veState) Visible
  }

-- | Open the selected entity with the command given from the editor widget.
--
openWithCommand :: Action 'ViewMail 'MailAttachmentOpenWithEditor (T.Next AppState)
openWithCommand =
  Action
    { _aDescription = ["ask for command to open attachment"]
    , _aAction =
        \s ->
          let cmd = view (asMailView . mvOpenCommand . E.editContentsL . to (T.unpack . currentLine)) s
           in case cmd of
            [] -> Brick.continue $ setError (GenericError "Empty command") s
            (x:xs) -> Brick.suspendAndResume
                      $ liftIO
                      $ openCommand' s (MailcapHandler (Process (x :| xs) []) IgnoreOutput DiscardTempfile)
    }

-- | Pipe the selected entity to the command given from the editor widget.
--
pipeToCommand :: Action 'ViewMail 'MailAttachmentPipeToEditor (T.Next AppState)
pipeToCommand =
  Action
  { _aDescription = ["pipe to external command"]
  , _aAction = \s ->
    let cmd = view (asMailView . mvPipeCommand . E.editContentsL . to (T.unpack . currentLine)) s
    in Brick.suspendAndResume $ liftIO $ pipeCommand' s cmd
  }

-- | Chain sequences of actions to create a keybinding
--
chain :: Action v ctx AppState -> Action v ctx a -> Action v ctx a
chain (Action d1 f1) (Action d2 f2) = Action (d1 <> d2) (f1 >=> f2)

-- | /Special/ form of chain allowing to sequencing actions registered
-- for a different view/widget. This is useful to perform actions on
-- widget focus changes.
--
chain'
    :: forall ctx ctx' a v v'.
       (HasName ctx', HasViewName v', ViewTransition v v')
    => Action v ctx AppState
    -> Action v' ctx' a
    -> Action v ctx a
chain' (Action d1 f1) (Action d2 f2) =
  Action (d1 <> d2) (f1 >=> switchMode >=> f2)
  where
    switchMode s = pure $ s &
      transitionHook (Proxy :: Proxy v) (Proxy :: Proxy v')
      . over (asViews . vsFocusedView) (Brick.focusSetCurrent (viewname (Proxy :: Proxy v')))
      . set (asViews . vsViews . at (viewname (Proxy :: Proxy v')) . _Just . vFocus) (name (Proxy :: Proxy ctx'))

done :: forall a v. (HasViewName v, Completable a) => Action v a AppState
done = Action ["apply"] (complete (Proxy :: Proxy a))

abort :: forall a v. (HasViewName v, Resetable v a) => Action v a AppState
abort = Action ["cancel"] (reset (Proxy :: Proxy v) (Proxy :: Proxy a))

-- $keybinding_actions
-- These actions are used to sequence other actions together. Think of
-- it like glue functions.
--

-- | Used to switch the focus from one widget to another on the same view.
--
focus :: forall a v. (HasViewName v, HasName a, Focusable v a) => Action v a AppState
focus = Action
  ["switch mode to " <> T.pack (show (name (Proxy :: Proxy a)))]
  (switchFocus (Proxy :: Proxy v) (Proxy :: Proxy a))

-- | A no-op action returning the current 'AppState'. This action can
-- be used at the start of a sequence with an immediate switch of
-- focus to a different widget (see 'focus').
--
noop :: Action v ctx AppState
noop = Action mempty pure

scrollUp :: forall ctx v. (Scrollable ctx) => Action v ctx AppState
scrollUp = Action
  { _aDescription = ["scroll up"]
  , _aAction = (<$ Brick.vScrollBy (makeViewportScroller (Proxy :: Proxy ctx)) (-1))
  }

scrollDown :: forall ctx v. (Scrollable ctx) => Action v ctx AppState
scrollDown = Action
  { _aDescription = ["scroll down"]
  , _aAction = \s -> s <$ Brick.vScrollBy (makeViewportScroller (Proxy :: Proxy ctx)) 1
  }

scrollPageUp :: forall ctx v. (Scrollable ctx) => Action v ctx AppState
scrollPageUp = Action
  { _aDescription = ["page up"]
  , _aAction = \s -> Brick.vScrollPage (makeViewportScroller (Proxy :: Proxy ctx)) T.Up >> pure s
  }

scrollPageDown :: forall ctx v. (Scrollable ctx) => Action v ctx AppState
scrollPageDown = Action
  { _aDescription = ["page down"]
  , _aAction = \s -> Brick.vScrollPage (makeViewportScroller (Proxy :: Proxy ctx)) T.Down >> pure s
  }

scrollNextWord :: forall ctx v. (Scrollable ctx) => Action v ctx AppState
scrollNextWord =
  Action
    { _aDescription = ["find next word in mail body"]
    , _aAction =
        \s -> Brick.vScrollToBeginning (makeViewportScroller (Proxy :: Proxy ctx))
          *> if has (asMailView . mvScrollSteps) s
               then
                 let s' = s & over (asMailView . mvScrollSteps) Brick.focusNext
                     nextLine = preview (asMailView . mvScrollSteps
                                         . to Brick.focusGetCurrent . _Just . _1) s'
                     scrollBy = view (non 0) nextLine
                 in Brick.vScrollBy (makeViewportScroller (Proxy :: Proxy ctx)) scrollBy >> pure s'
               else pure $ s & setError (GenericError "No match")
    }

-- | Removes any highlighting done by searching in the body text
--
removeHighlights :: Action 'ViewMail 'ScrollingMailView AppState
removeHighlights =
  Action
    { _aDescription = ["remove search results highlights"]
    , _aAction = pure . resetMatchingWords
    }

displayMail :: Action 'ViewMail 'ScrollingMailView AppState
displayMail =
    Action
    { _aDescription = ["display an e-mail"]
    , _aAction = \s -> do
        resetScrollState
        liftIO $ updateStateWithParsedMail s
          >>= updateReadState (RemoveTag $ view (asConfig . confNotmuch . nmNewTag) s)
    }
  where resetScrollState = Brick.vScrollToBeginning (makeViewportScroller (Proxy :: Proxy 'ScrollingMailView))

-- | Sets the mail list to the mails for the selected thread. Does
-- not select a mail; a movement action such as 'displayNextUnread'
-- should follow this action.
--
displayThreadMails :: Action 'Threads 'ListOfThreads AppState
displayThreadMails =
    Action
    { _aDescription = ["display an e-mail for threads"]
    , _aAction = liftIO . setMailsForThread
    }

setUnread :: Action 'ViewMail 'ScrollingMailView AppState
setUnread =
    Action
    { _aDescription = ["toggle unread"]
    , _aAction = \s -> liftIO $ updateReadState (AddTag $ view (asConfig . confNotmuch . nmNewTag) s) s
    }

listUp
  :: forall v ctx.  (HasList ctx, Foldable (T ctx), L.Splittable (T ctx))
  => Action v ctx AppState
listUp = Action ["list up"] (pure . over (list (Proxy :: Proxy ctx)) L.listMoveUp)

listDown
  :: forall v ctx.  (HasList ctx, Foldable (T ctx), L.Splittable (T ctx))
  => Action v ctx AppState
listDown = Action ["list down"] (pure . over (list (Proxy :: Proxy ctx)) L.listMoveDown)

listJumpToStart
  :: forall v ctx.  (HasList ctx, Foldable (T ctx), L.Splittable (T ctx))
  => Action v ctx AppState
listJumpToStart = Action ["list top"] (pure . over (list (Proxy :: Proxy ctx)) (L.listMoveTo 0))

listJumpToEnd
  :: forall v ctx.  (HasList ctx, Foldable (T ctx), L.Splittable (T ctx))
  => Action v ctx AppState
listJumpToEnd = Action ["list bottom"] (pure . over (list (Proxy :: Proxy ctx)) (L.listMoveTo (-1)))

-- | Action used to either start a composition of a new mail or switch
-- the view to the composition editor if we've already been editing a new
-- mail. The use case here is to continue editing an e-mail while
-- still having the ability to browse existing e-mails.
--
switchComposeEditor :: Action 'Threads 'ListOfThreads AppState
switchComposeEditor =
    Action
    { _aDescription = ["switch to compose editor"]
    , _aAction = \s -> if has (asCompose . cAttachments . traversed) s
                          then pure $ over (asViews . vsFocusedView) (Brick.focusSetCurrent ComposeView) s
                          else pure s
    }

-- | Update the 'AppState' with a quoted form of the first preferred
-- entity in order to reply to the e-mail.
--
replyMail :: Action 'ViewMail 'ScrollingMailView AppState
replyMail =
    Action
    { _aDescription = ["reply to an e-mail"]
    , _aAction = pure . replyToMail
    }

-- | Toggles whether we want to show all headers from an e-mail or a
-- filtered list in the 'AppState'.
--
toggleHeaders :: Action 'ViewMail 'ScrollingMailView AppState
toggleHeaders = Action
  { _aDescription = ["toggle mail headers"]
  , _aAction = pure . go
  }
  where
    go :: AppState -> AppState
    go s = case view (asMailView . mvHeadersState) s of
      Filtered -> set (asMailView . mvHeadersState) ShowAll s
      ShowAll -> set (asMailView . mvHeadersState) Filtered s

-- | Apply given tag operations on the currently selected thread or
-- mail.
--
setTags :: [TagOp] -> Action v ctx AppState
setTags ops =
    Action
    { _aDescription = ["apply given tags"]
    , _aAction = \s -> case focusedViewWidget s of
          ListOfMails -> selectedItemHelper (asMailIndex . miListOfMails) s (manageMailTags s ops)
          _ -> selectedItemHelper (asMailIndex . miListOfThreads) s (manageThreadTags s ops)
    }

-- | Reloads the list of threads by executing the notmuch query given
-- by the search widget.
--
reloadList :: Action 'Threads 'ListOfThreads AppState
reloadList = Action ["reload list of threads"] applySearch

-- | Selects the next unread mail in a thread.
--
selectNextUnread :: Action 'ViewMail 'ListOfMails AppState
selectNextUnread =
  Action { _aDescription = ["select next unread"]
         , _aAction = \s ->
           let
             -- find by unread tag...
             p = Notmuch.hasTag (view (asConfig . confNotmuch . nmNewTag) s)
             -- but if there is no resulting selection, move to the
             -- last element in the list
             f l = maybe (L.listMoveTo (-1) l) (const l) (view L.listSelectedL l)
           in pure $ over (asMailIndex . miListOfMails) (f . L.listFindBy p) s
         }

-- | Selects a list item. Currently only used in the file browser to
-- select a file for attaching.
--
toggleListItem :: Action v 'ListOfFiles AppState
toggleListItem =
    Action
    { _aDescription = ["toggle selected state of a list item"]
    , _aAction = \s ->
                      maybe
                          (pure s)
                          (\i -> pure $ over (asFileBrowser . fbEntries . L.listElementsL . ix i . _1) not s)
                          (view (asFileBrowser . fbEntries . L.listSelectedL) s)
    }

-- | Delete an attachment from a mail currently being composed.
--
delete :: Action 'ComposeView 'ComposeListOfAttachments AppState
delete =
    Action
    { _aDescription = ["delete entry"]
    , _aAction = \s ->
                      if view (asCompose . cAttachments . L.listElementsL . to length) s < 2
                          then pure $ setError (GenericError "You may not remove the only attachment") s
                          else let sel = view (asCompose . cAttachments . L.listSelectedL) s
                               in pure $ over (asCompose . cAttachments) (\l -> maybe l (`L.listRemove` l) sel) s
    }

-- | Go to the parent directory.
--
parentDirectory :: Action 'FileBrowser 'ListOfFiles AppState
parentDirectory = Action ["go to parent directory"]
                      (\s ->
                       let fp = view (asFileBrowser .fbSearchPath . E.editContentsL . to currentLine . to takeDirectory) s
                           s' = over (asFileBrowser . fbSearchPath . E.editContentsL)
                                (insertMany fp . clearZipper) s
                       in ($ s')
                          <$> (either setError updateBrowseFileContents
                               <$> runExceptT (view (asFileBrowser . fbSearchPath . E.editContentsL . to currentLine . to listDirectory') s'))
                      )

-- | Open a directory and set the contents in the 'AppState'.
--
enterDirectory :: Action 'FileBrowser 'ListOfFiles AppState
enterDirectory =
  Action
  { _aDescription = ["enter directory"]
  , _aAction = \s -> liftIO $ case view (asFileBrowser . fbEntries . to L.listSelectedElement) s of
      Just (_, item) ->
        case view _2 item of
          Directory _ ->
            let fp = fullpath s item
                s' = over (asFileBrowser . fbSearchPath . E.editContentsL) (insertMany fp. clearZipper) s
            in ($ s') <$> (either setError updateBrowseFileContents
                           <$> runExceptT (view (asFileBrowser . fbSearchPath . E.editContentsL . to currentLine . to listDirectory') s'))
          _ -> pure s
      Nothing -> pure s
  }

-- | Adds all selected files as attachments to the e-mail.
--
createAttachments :: Action 'FileBrowser 'ListOfFiles AppState
createAttachments =
    Action
        ["adds selected files as attachments"]
        (\s ->
              if isFileUnderCursor $ L.listSelectedElement $ view (asFileBrowser . fbEntries) s
              then liftIO $ makeAttachmentsFromSelected s
              else pure s)

-- | Action to deal with a choice from the confirmation dialog.
--
handleConfirm :: Action 'ComposeView 'ConfirmDialog AppState
handleConfirm =
  Action
    ["handle confirmation"]
    (liftIO . keepOrDiscardDraft)

-- | Edit an e-mail as a new mail. This is typically used by saving a
-- mail under composition for later and continuing the draft. Another
-- use case can be editing an already sent mail in order to send it to
-- anther recipient.
--
composeAsNew :: Action 'ViewMail 'ScrollingMailView AppState
composeAsNew =
  Action
    ["edit mail as new"]
    (\s ->
       case preview
              (asMailIndex .
               miListOfMails . to L.listSelectedElement . _Just . _2)
              s of
         Nothing -> pure $ setError (GenericError "No mail selected") s
         Just mail ->
           let pmail = view (asMailView . mvMail) s
               dbpath = view (asConfig . confNotmuch . nmDatabase) s
               charsets = view (asConfig . confCharsets) s
            in liftIO $
               either
                 (`setError` s)
                 (set asCompose (newComposeFromMail charsets pmail)) <$>
               runExceptT
                 (Notmuch.mailFilepath mail dbpath >>=
                  Notmuch.unindexFilePath dbpath >>
                  pure s))


-- Function definitions for actions
--

-- | Traverse and make attachments from the selected files in the file
-- browser.
makeAttachmentsFromSelected :: AppState -> IO AppState
makeAttachmentsFromSelected s = do
  parts <- traverse (\x -> createAttachmentFromFile (mimeType x) (makeFullPath x)) (selectedFiles (view (asFileBrowser . fbEntries) s))
  pure $ s & over (asCompose . cAttachments) (go parts)
    . over (asViews . vsFocusedView) (Brick.focusSetCurrent ComposeView)
    . set (asViews . vsViews . at ComposeView . _Just . vFocus) ComposeListOfAttachments
  where
    go :: [MIMEMessage] -> L.List Name MIMEMessage -> L.List Name MIMEMessage
    go parts l = foldr (upsertPart charsets) l parts
    charsets = view (asConfig . confCharsets) s
    makeFullPath path = currentLine (view (asFileBrowser . fbSearchPath . E.editContentsL) s) </> path

-- | Determine if the selected directory entry is a file or not. We do
-- not support adding entire directories to the e-mail.
--
isFileUnderCursor :: Maybe (a, (b, FileSystemEntry)) -> Bool
isFileUnderCursor i = maybe False isFile (preview (_Just . _2 . _2) i)
  where isFile (File _) = True
        isFile _ = False

-- | Construct the full path to the attachment. The file browser only
-- lists the file names (otherwise we wouldn't have the ability to
-- display the full paths for deeper file hirarchies). However when
-- attaching the file, we need the full paths so we can find, edit and
-- update the attachments later.
--
fullpath :: AppState -> (a, FileSystemEntry) -> FilePath
fullpath s i = currentLine (view (asFileBrowser . fbSearchPath . E.editContentsL) s) </> view (_2 . fsEntryName) i

-- | Update the result of a file system listing in the 'AppState'
--
updateBrowseFileContents :: [FileSystemEntry] -> AppState -> AppState
updateBrowseFileContents contents s =
  let contents' = view vector ((False, ) <$> contents)
  in over (asFileBrowser . fbEntries) (L.listReplace contents' (Just 0)) s

-- | Take the notmuch query given by the user and update the
-- 'AppState' with notmuch query result.
--
applySearch :: (MonadIO m) => AppState -> m AppState
applySearch s = do
  r <- runExceptT (Notmuch.getThreads searchterms (view (asConfig . confNotmuch) s))
  t <- liftIO (zonedTimeToUTC <$> getZonedTime)
  case r of
    Left e -> pure $ setError e s
    Right threads -> set asLocalTime t . updateList threads <$> notifyNumThreads s threads
   where searchterms = currentLine $ view (asMailIndex . miSearchThreadsEditor . E.editContentsL) s
         updateList vec =
           over (asMailIndex . miThreads . listList) (L.listReplace vec (Just 0))
           . set (asMailIndex . miThreads . listLength) Nothing

-- | Fork a thread to compute the length of the container and send a
-- NotifyNumThreads event.  'seq' ensures that the work is actually
-- done by the spawned thread.  Increments the generation and updates
-- the 'AppState' with it.
notifyNumThreads :: (MonadIO m, Foldable t) => AppState -> t a -> m AppState
notifyNumThreads s l =
  let
    len = length l
    nextGen = views (asMailIndex . miListOfThreadsGeneration) nextGeneration s
    s' = set (asMailIndex . miListOfThreadsGeneration) nextGen s
    go = len `seq` writeBChan (view (asConfig . confBChan) s) (NotifyNumThreads len nextGen)
  in
    liftIO $ forkIO go $> s'

-- | Update the Application state with all mails found for the
-- currently selected thread.
--
setMailsForThread :: AppState -> IO AppState
setMailsForThread s = selectedItemHelper (asMailIndex . miListOfThreads) s $ \t ->
  let dbpath = view (asConfig . confNotmuch . nmDatabase) s
      updateThreadMails vec =
        over (asMailIndex . miMails . listList) (L.listReplace vec Nothing)
        . set (asMailIndex . miMails . listLength) (Just (length vec))
  in either setError updateThreadMails <$> runExceptT (Notmuch.getThreadMessages dbpath t)

-- | Helper function to either show an error if no list item is
-- selected or apply the given function.
--
selectedItemHelper
    :: (Applicative f, Foldable t, L.Splittable t)
    => Getting (L.GenericList n t a) AppState (L.GenericList n t a)
    -> AppState
    -> (a -> f (AppState -> AppState))
    -> f AppState
selectedItemHelper l s func =
  ($ s) <$> case L.listSelectedElement (view l s) of
  Just (_, a) -> func a
  Nothing -> pure $ setError (GenericError "No item selected.")

-- | Retrieve the given tag operations from the given editor widget
-- and parse them.
--
getEditorTagOps :: HasEditor n => Proxy n -> AppState -> Either Error [TagOp]
getEditorTagOps p s =
  let contents = (foldr (<>) "" $ E.getEditContents $ view (editorL p) s)
  in parseTagOps contents

-- | Apply given tag operations on all mails
--
applyTagOps
  :: (Traversable t, MonadIO m)
  => [TagOp]
  -> t NotmuchMail
  -> AppState
  -> m (Either Error (t NotmuchMail))
applyTagOps ops mails s =
  let dbpath = view (asConfig . confNotmuch . nmDatabase) s
  in runExceptT (Notmuch.messageTagModify dbpath ops mails)

updateStateWithParsedMail :: AppState -> IO AppState
updateStateWithParsedMail s =
  selectedItemHelper (asMailIndex . miListOfMails) s $ \m ->
    either
      (\e ->
         setError e .
         over (asViews . vsFocusedView) (Brick.focusSetCurrent Threads))
      (\(pmail, mbody) s' ->
         s' &
         set (asMailView . mvMail) (Just pmail) .
         set (asMailView . mvBody) mbody .
         over (asViews . vsFocusedView) (Brick.focusSetCurrent ViewMail) .
         set (asMailView . mvAttachments) (setEntities pmail)) <$>
    runExceptT
      (parseMail m (view (asConfig . confNotmuch . nmDatabase) s) >>=
       bodyToDisplay s textwidth charsets preferredContentType)
  where
    charsets = view (asConfig . confCharsets) s
    textwidth = view (asConfig . confMailView . mvTextWidth) s
    setEntities m =
      L.list MailListOfAttachments (view vector $ toListOf entities m) 0
    preferredContentType =
      view (asConfig . confMailView . mvPreferredContentType) s

-- | Tag the currently selected mail as /read/. This is reflected as a
-- visual change in the UI.
--
updateReadState :: TagOp -> AppState -> IO AppState
updateReadState op s =
  -- Also update the thread to reflect the change. We used
  -- to pull the thread out of the database again when we
  -- navigated back to the index of threads, but does not
  -- always garantee an updated tag list. See #249
  over (asMailIndex . miListOfThreads) (L.listModify (Notmuch.tagItem [op]))
  <$> selectedItemHelper (asMailIndex . miListOfMails) s (manageMailTags s [op])

manageMailTags
    :: MonadIO m
    => AppState
    -> [TagOp]
    -> NotmuchMail
    -> m (AppState -> AppState)
manageMailTags s tagop m =
  either setError (over (asMailIndex . miListOfMails) . L.listModify . const . runIdentity)
  <$> applyTagOps tagop (Identity m) s

-- | Convenience function to set an error state in the 'AppState'
--
setError :: Error -> AppState -> AppState
setError = set asError . Just

-- | Update the 'AppState' with a quoted version of the currently
-- selected mail in order to reply to it.
--
replyToMail :: AppState -> AppState
replyToMail s =
  case view (asMailView . mvMail) s of
    Nothing ->
      s &
      over (asViews . vsFocusedView) (Brick.focusSetCurrent Threads) .
      setError (GenericError "No mail selected for replying")
    Just pmail ->
      let quoted = toQuotedMail mbody pmail
          mbody = view (asMailView . mvBody) s
          charsets = view (asConfig . confCharsets) s
       in s &
          over (asCompose . cTo . E.editContentsL) (insertMany (getTo quoted) . clearZipper)
          . over (asCompose . cFrom . E.editContentsL) (insertMany (getFrom quoted) . clearZipper)
          . over (asCompose . cSubject . E.editContentsL) (insertMany (getSubject quoted) . clearZipper)
          . over (asCompose . cAttachments) (upsertPart charsets quoted)

-- | Build the MIMEMessage, sanitize filepaths, serialize and send it
--
sendMail :: AppState -> T.EventM Name AppState
sendMail s =
  let charsets = view (asConfig . confCharsets) s
      maildir = view (asConfig . confNotmuch . nmDatabase) s
      sentTag = view (asConfig . confNotmuch . nmSentTag) s
      send = do
        (bs, randomg) <- over _1 (renderMessage . sanitizeMail charsets) <$> buildMail s
        s' <- liftIO $ trySendAndCatch randomg bs s
        sentFP <- createSentFilePath maildir
        Notmuch.indexMail bs maildir sentFP sentTag
        pure s'
   in either (`setError` s) id <$> runExceptT send

-- | Build the MIMEMessage from the compose editor fields including Date and Boundary
--
buildMail :: (MonadError Error m, MonadIO m) => AppState -> m (MIMEMessage, String)
buildMail s = do
    dateTimeNow <- liftIO getCurrentTime
    let to' = either (pure []) id $ parseOnly addressList $ T.encodeUtf8 $ T.unlines $ E.getEditContents $ view (asCompose . cTo) s
        from = either (pure []) id $ parseOnly mailboxList $ T.encodeUtf8 $ T.unlines $ E.getEditContents $ view (asCompose . cFrom) s
        subject = T.unlines $ E.getEditContents $ view (asCompose . cSubject) s
        (b, l') = splitAt 50 $ view (asConfig . confBoundary) s
        attachments' = toListOf (asCompose . cAttachments . L.listElementsL . traversed) s
        mail = case attachments' of
          x:xs  -> Just $ createMultipartMixedMessage (C8.pack b) (x:|xs)
          _     -> firstOf (asCompose . cAttachments . L.listElementsL . traversed) s
    case mail of
        Nothing -> throwError (GenericError "Black hole detected")
        (Just m) -> let m' = m
                            & set (headers . at "Subject") (Just $ T.encodeUtf8 subject)
                            . set (headers . at "From") (Just $ renderMailboxes from)
                            . set (headers . at "To") (Just $ renderAddresses to')
                            . set (headers . at "Date") (Just $ renderRFC5422Date dateTimeNow)
                    in pure (m', l')

-- Handler to send the mail, but catch and show an error if it happened. This is
-- really a TODO to improve this, since ideally we can do this in the
-- cvSendMailCmd. However what prevented me from simply moving there is using
-- the MonadError typeclass. Using that, the typevariable has to be carried into
-- the configuration as another? type variable. So all in all it was non
-- trivial.
-- See #200
trySendAndCatch :: String -> B.ByteString -> AppState -> IO AppState
trySendAndCatch l' m s = do
    let cmd = view (asConfig . confComposeView . cvSendMailCmd) s
        sendmail = view (asConfig . confComposeView . cvSendMailPath) s
        defMailboxes = view (asConfig . confComposeView . cvIdentities) s
    catch
        (cmd sendmail m $> (s
         & set asCompose (initialCompose defMailboxes)
         . set (asConfig . confBoundary) l'))
        (\e ->
              let err = show (e :: IOException)
              in pure $ s & setError (SendMailError err))

-- | santize the mail before we send it out
-- Note: currently only strips away path names from files
sanitizeMail :: CharsetLookup -> MIMEMessage -> MIMEMessage
sanitizeMail charsets =
  over (attachments . headers . contentDisposition . traversed . filename charsets) takeFileName

initialCompose :: [Mailbox] -> Compose
initialCompose mailboxes =
  Compose
    (E.editorText ComposeFrom (Just 1) (AddressText.renderMailboxes mailboxes))
    (E.editorText ComposeTo (Just 1) "")
    (E.editorText ComposeSubject (Just 1) "")
    T.empty
    (L.list ComposeListOfAttachments mempty 1)
    initialDraftConfirmDialog

-- | Set the compose state from an existing mail. This is typically
-- used for re-editing draft mails.
newComposeFromMail :: CharsetLookup -> Maybe MIMEMessage -> Compose
newComposeFromMail charsets m =
  let subject =
        preview (_Just . headers . at "subject" . _Just . to decodeLenient) m
      from = preview (_Just . headers . at "from" . _Just . to decodeLenient) m
      to' = preview (_Just . headers . at "to" . _Just . to decodeLenient) m
      attachments' =
        view vector $ toMIMEMessage charsets <$> toListOf (_Just . entities) m
      orEmpty = view (non "")
   in Compose
        (E.editorText ComposeFrom (Just 1) (orEmpty from))
        (E.editorText ComposeTo (Just 1) (orEmpty to'))
        (E.editorText ComposeSubject (Just 1) (orEmpty subject))
        T.empty
        (L.list ComposeListOfAttachments attachments' 1)
        initialDraftConfirmDialog

initialDraftConfirmDialog :: Dialog ConfirmDraft
initialDraftConfirmDialog = dialog (Just "Keep draft?") (Just (0, [("Keep", Keep), ("Discard", Discard)])) 50

-- | Serialise the WireEntity and write it to a temporary file. If no WireEntity
-- exists (e.g. composing a new mail) just use the empty file. When the
-- serialising fails, we return an error. Once the editor exits, read the
-- contents from the temporary file, delete it and create a MIME message out of
-- it. Set it in the Appstate.
invokeEditor' :: AppState -> IO AppState
invokeEditor' s =
  let maybeEntity = preview (asCompose . cAttachments . to L.listSelectedElement
                             . _Just . _2 . to getTextPlainPart . _Just) s
      maildir = view (asConfig . confNotmuch . nmDatabase) s
      cmd = view (asConfig . confEditor) s
      updatePart = over (asCompose . cAttachments) . upsertPart charsets . createTextPlainMessage
      mkEntity :: (MonadError Error m) => m B.ByteString
      mkEntity = maybe (pure mempty) entityToBytes maybeEntity
      entityCmd = EntityCommand handleExitCodeTempfileContents (draftFileResoure maildir) (\_ fp -> proc cmd [fp]) tryReadProcessStderr
      charsets = view (asConfig . confCharsets) s
  in
    either (`setError` s) (`updatePart` s)
    <$> runExceptT (mkEntity >>= runEntityCommand . entityCmd)

-- | Write the serialised WireEntity to a temporary file. Pass the FilePath of
-- the temporary file to the command. Do not remove the temporary file, so
-- programs using sub-shells will be able to read the temporary file. Return an
-- error if either the WireEntity doesn't exist (e.g. not selected) or it can
-- not be serialised.
openCommand' :: AppState -> MailcapHandler -> IO AppState
openCommand' s cmd =
  let
    mkConfig :: (MonadError Error m, MonadIO m) => WireEntity -> m (EntityCommand m FilePath)
    mkConfig =
      let con = EntityCommand
            handleExitCodeThrow
            (tmpfileResource (view mhKeepTemp cmd))
            (\_ fp -> toProcessConfigWithTempfile (view mhMakeProcess cmd) fp)
            tryReadProcessStderr
      in fmap con . entityToBytes
  in either (`setError` s) (const s)
      <$> runExceptT (selectedAttachmentOrError s >>= mkConfig >>= runEntityCommand)

-- | Pass the serialized WireEntity to a Bytestring as STDIN to the process. No
-- temporary file is used. If either no WireEntity exists (e.g. none selected)
-- or it can not be serialised an error is returned.
pipeCommand' :: AppState -> FilePath -> IO AppState
pipeCommand' s cmd
  | null cmd = pure $ s & setError (GenericError "Empty command")
  | otherwise =
    let
      mkConfig :: (MonadError Error m, MonadIO m) => WireEntity -> m (EntityCommand m ())
      mkConfig =
        let con = EntityCommand
              handleExitCodeThrow
              emptyResource
              (\b _ -> setStdin (byteStringInput $ LB.fromStrict b) (proc cmd []))
              tryReadProcessStderr
        in fmap con . entityToBytes
     in either (`setError` s) (const s)
        <$> runExceptT (selectedAttachmentOrError s >>= mkConfig >>= runEntityCommand)

selectedAttachmentOrError :: MonadError Error m => AppState -> m WireEntity
selectedAttachmentOrError =
  maybe (throwError $ GenericError "No attachment selected") pure
  . preview (asMailView . mvAttachments . to L.listSelectedElement . _Just . _2)

editAttachment :: AppState -> IO AppState
editAttachment s =
    case L.listSelectedElement $ view (asCompose . cAttachments) s of
        Nothing -> pure $ setError (GenericError "No file selected to edit") s
        Just (_, m) -> case preview (headers . contentDisposition . folded . dispositionType) m of
          (Just Inline) -> invokeEditor' s
          _ -> pure $ setError (GenericError "Not implemented. See #182") s

-- | Either inserts or updates a part in a list of attachments. This
-- is needed when editing parts during composition of an e-mail.
--
upsertPart ::
     CharsetLookup
  -> MIMEMessage
  -> L.List Name MIMEMessage
  -> L.List Name MIMEMessage
upsertPart charsets newPart l =
  case L.listSelectedElement l of
    Nothing -> L.listInsert 0 newPart l
    Just (_, part) ->
      if view (headers . contentDisposition . folded . filename charsets) part
          == view (headers . contentDisposition . folded . filename charsets) newPart
      then
        -- replace
        L.listModify (const newPart) l
      else
        -- append
        l & over L.listElementsL (`snoc` newPart)
             . set L.listSelectedL (Just (view (L.listElementsL . to length) l))

getTextPlainPart :: MIMEMessage -> Maybe WireEntity
getTextPlainPart = firstOf (entities . filtered f)
  where
  f = matchContentType "text" (Just "plain") . view (headers . contentType)

mimeType :: FilePath -> ContentType
mimeType x = let parsed = parseOnly parseContentType $ defaultMimeLookup (T.pack x)
             in either (const contentTypeApplicationOctetStream) id parsed

manageThreadTags
    :: MonadIO m
    => AppState
    -> [TagOp]
    -> NotmuchThread
    -> m (AppState -> AppState)
manageThreadTags s ops t =
  let update ops' _ = over (asMailIndex . miListOfThreads) (L.listModify (Notmuch.tagItem ops'))
  in getMailsForThread t s
     >>= \ms -> applyTagOps ops ms s
     >>= either (pure . setError) (pure . update ops)

getMailsForThread
    :: MonadIO f
    => NotmuchThread
    -> AppState
    -> f (Vector.Vector NotmuchMail)
getMailsForThread ts s =
  let dbpath = view (asConfig . confNotmuch . nmDatabase) s
  in
    either (const mempty) id
    <$> runExceptT (Notmuch.getThreadMessages dbpath ts)


keepOrDiscardDraft :: AppState -> IO AppState
keepOrDiscardDraft s =
  case view (asCompose . cKeepDraft . to dialogSelection) s of
    Just Keep ->
      let maildir = view (asConfig . confNotmuch . nmDatabase) s
       in either (`setError` s) (clearMailComposition . setError (GenericError "Draft saved")) <$>
          runExceptT (keepDraft s maildir)
    _ -> pure $ s & clearMailComposition . setError (GenericError "Draft discarded")

keepDraft ::
     (MonadMask m, MonadError Error m, MonadIO m)
  => AppState
  -> FilePath
  -> m AppState
keepDraft s maildir =
  let draftTag = view (asConfig . confNotmuch . nmDraftTag) s
  in do
    bs <- renderMessage . fst <$> buildMail s
    fp <- createDraftFilePath maildir
    Notmuch.indexMail bs maildir fp draftTag
    pure s

resetMatchingWords :: AppState -> AppState
resetMatchingWords =
  over (asMailView . mvBody) removeMatchingWords
  . over (asMailView . mvFindWordEditor . E.editContentsL) clearZipper
  . set (asMailView . mvScrollSteps) (Brick.focusRing [])
