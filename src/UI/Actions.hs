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
  Scrollable(..)
  , HasEditor(..)
  , HasName(..)
  , quit
  , focus
  , done
  , abort
  , noop
  , displayMail
  , displayThreadMails
  , setUnread
  , listUp
  , listDown
  , listJumpToEnd
  , listJumpToStart
  , switchComposeEditor
  , replyMail
  , scrollUp
  , scrollDown
  , scrollPageUp
  , scrollPageDown
  , toggleHeaders
  , initialCompose
  , continue
  , chain
  , chain'
  , setTags
  , invokeEditor
  , edit
  , reloadList
  , selectNextUnread
  , toggleListItem
  , enterDirectory
  , parentDirectory
  , createAttachments
  , delete
  , applySearch
  , openWithCommand
  , openAttachment
  , pipeToCommand
  ) where

import Data.Functor.Identity (Identity(..))

import qualified Brick
import Brick.BChan (writeBChan)
import qualified Brick.Focus as Brick
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Network.Mime (defaultMimeLookup)
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
        filtered, set, over, preview, view, views, (&), nullOf, firstOf,
        Getting, Lens')
import Control.Concurrent (forkIO)
import Control.Monad ((>=>))
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
        parseContentType, attachments, isAttachment, entities, matchContentType,
        contentType, mailboxList, renderMailboxes, addressList, renderAddresses,
        renderRFC5422Date, MIMEMessage, WireEntity, DispositionType(..),
        ContentType(..), Mailbox(..),
        CharsetLookup)
import qualified Storage.Notmuch as Notmuch
import Storage.ParsedMail
       (parseMail, getTo, getFrom, getSubject, toQuotedMail, entityToBytes)
import Types
import Error
import UI.Utils (selectedFiles, takeFileName)
import UI.Views
       (listOfMailsView, mailView, toggleLastVisibleWidget, indexView, resetView,
        focusedViewWidget)
import Purebred.Events (nextGeneration)
import Purebred.LazyVector (V)
import Purebred.Tags (parseTagOps)
import Purebred.System.Directory (listDirectory')
import Purebred.System.Process

class Scrollable (n :: Name) where
  makeViewportScroller :: Proxy n -> Brick.ViewportScroll Name

instance Scrollable 'ScrollingMailView where
  makeViewportScroller _ = Brick.viewportScroll ScrollingMailView

instance Scrollable 'ScrollingHelpView where
  makeViewportScroller _ = Brick.viewportScroll ScrollingHelpView

-- | contexts that have an editor
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


-- | An action - typically completed by a key press (e.g. Enter) - and it's
-- contents are used to be applied to an action.
--
-- For example: the user changes
-- the notmuch search terms to find a particular mail. To apply his changes, he
-- 'completes' his text entered by pressing Enter.
--
-- Another example is sending e-mail. So the complete action for the
-- ComposeEditor is sent, since that's at the end of the composition process.
--
class Completable (m :: Name) where
  complete :: Proxy m -> AppState -> T.EventM Name AppState

instance Completable 'SearchThreadsEditor where
  complete _ = applySearch

instance Completable 'ManageMailTagsEditor where
  complete _ s = liftIO $ over (asMailIndex . miMailTagsEditor . E.editContentsL) clearZipper <$> completeMailTags s

instance Completable 'ComposeListOfAttachments where
  complete _ = sendMail

completeMailTags :: AppState -> IO AppState
completeMailTags s =
    case getEditorTagOps (Proxy :: Proxy 'ManageMailTagsEditor) s of
        Left err -> pure $ setError err s
        Right ops' -> over (asMailIndex . miListOfThreads) (L.listModify (Notmuch.tagItem ops'))
                      <$> selectedItemHelper (asMailIndex . miListOfMails) s (manageMailTags s ops')

instance Completable 'ComposeTo where
  complete _ = pure . set (asViews . vsViews . at ComposeView . _Just . vWidgets . ix ComposeTo . veState) Hidden

instance Completable 'ComposeFrom where
  complete _ = pure . set (asViews . vsViews . at ComposeView . _Just . vWidgets . ix ComposeFrom . veState) Hidden

instance Completable 'ComposeSubject where
  complete _ = pure . set (asViews . vsViews . at ComposeView . _Just . vWidgets . ix ComposeSubject . veState) Hidden

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
               . set (asViews . vsViews . at ViewMail . _Just . vWidgets . ix MailAttachmentOpenWithEditor . veState) Hidden

instance Completable 'MailAttachmentPipeToEditor where
  complete _ = pure
               . set (asViews . vsViews . at ViewMail . _Just . vWidgets . ix MailAttachmentPipeToEditor . veState) Hidden

-- | Generalisation of reset actions, whether they reset editors back to their
-- initial state or throw away composed, but not yet sent mails.
--
class Resetable (v :: ViewName) (m :: Name) where
  reset :: Proxy v -> Proxy m -> AppState -> T.EventM Name AppState

instance Resetable 'Threads 'SearchThreadsEditor where
  reset _ _ = pure

instance Resetable 'Mails 'ManageMailTagsEditor where
  reset _ _ = pure . over (asMailIndex . miMailTagsEditor . E.editContentsL) clearZipper

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
                . set (asViews . vsViews . at ComposeView . _Just . vWidgets . ix ComposeFrom . veState) Hidden

instance Resetable 'ComposeView 'ComposeTo where
  reset _ _ s = pure $ s & over (asCompose . cTo . E.editContentsL) (revertEditorContents s)
                . set (asViews . vsViews . at ComposeView . _Just . vWidgets . ix ComposeTo . veState) Hidden

instance Resetable 'ComposeView 'ComposeSubject where
  reset _ _ s = pure $ s & over (asCompose . cSubject . E.editContentsL) (revertEditorContents s)
                . set (asViews . vsViews . at ComposeView . _Just . vWidgets . ix ComposeTo . veState) Hidden

revertEditorContents :: AppState -> TextZipper T.Text -> TextZipper T.Text
revertEditorContents s z = let saved = view (asCompose . cTemp) s
                               replace = insertMany saved . clearZipper
                           in replace z

instance Resetable 'ComposeView 'ComposeListOfAttachments where
  reset _ _ = pure . clearMailComposition

instance Resetable 'FileBrowser 'ManageFileBrowserSearchPath where
  reset _ _ = pure . over (asFileBrowser . fbSearchPath . E.editContentsL) clearZipper

instance Resetable 'ViewMail 'MailListOfAttachments where
  reset _ _ = pure . set (asViews . vsViews . at ViewMail . _Just . vWidgets . ix MailListOfAttachments . veState) Hidden

instance Resetable 'ViewMail 'MailAttachmentOpenWithEditor where
  reset _ _ = pure . over (asMailView . mvOpenCommand . E.editContentsL) clearZipper
            . set (asViews . vsViews . at ViewMail . _Just . vWidgets . ix MailAttachmentOpenWithEditor . veState) Hidden

instance Resetable 'ViewMail 'MailAttachmentPipeToEditor where
  reset _ _ = pure . over (asMailView . mvPipeCommand . E.editContentsL) clearZipper
            . set (asViews . vsViews . at ViewMail . _Just . vWidgets . ix MailAttachmentPipeToEditor . veState) Hidden

clearMailComposition :: AppState -> AppState
clearMailComposition s =
    let mailboxes = AddressText.renderMailboxes $ view (asConfig . confComposeView . cvIdentities) s
    in s
        -- insert default from addresses
        & over (asCompose . cFrom . E.editContentsL) (insertMany mailboxes . clearZipper)
        -- clear editor contents for other fields
        . over (asCompose . cTo . E.editContentsL) clearZipper
        . over (asCompose . cSubject . E.editContentsL) clearZipper
        . over (asCompose . cAttachments) L.listClear
        -- reset the UI
        -- Note: Only replace the last widget on the Threads view with the
        -- SearchThreadsEditor. This is important if we abort mail composition
        -- when we're still looking at a list of threads. The composition can
        -- also be aborted in the composition editor, with which we would not
        -- want to replace anything. Implement #181 to fix this.
        . toggleLastVisibleWidget SearchThreadsEditor

-- | Generalisation of focus changes between widgets on the same "view"
-- expressed with the mode in the application state.
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

instance Focusable 'Mails 'ManageMailTagsEditor where
  switchFocus _ _ = pure . over (asMailIndex . miMailTagsEditor . E.editContentsL) clearZipper
                    . set (asViews . vsViews . at Mails . _Just . vFocus) ManageMailTagsEditor

instance Focusable 'ViewMail 'ManageMailTagsEditor where
  switchFocus _ _ s = pure $ s &
                      set (asViews . vsViews . at ViewMail . _Just . vWidgets . ix ManageMailTagsEditor . veState) Visible
                      . set (asViews . vsViews . at ViewMail . _Just . vFocus) ManageMailTagsEditor

instance Focusable 'ViewMail 'ScrollingMailView where
  switchFocus _ _ = pure . set (asViews. vsViews . at ViewMail . _Just . vFocus) ScrollingMailView

instance Focusable 'Mails 'ListOfMails where
  switchFocus _ _ = pure

instance Focusable 'Mails 'ComposeFrom where
  switchFocus = focusComposeFrom

instance Focusable 'ViewMail 'ListOfMails where
  switchFocus _ _ = pure . set (asViews . vsViews . at ViewMail . _Just . vFocus) ListOfMails

instance Focusable 'ViewMail 'MailListOfAttachments where
  switchFocus _ _ = pure . set (asViews . vsViews . at ViewMail . _Just . vFocus) MailListOfAttachments
                    . set (asViews . vsViews . at ViewMail . _Just . vWidgets . ix MailListOfAttachments . veState) Visible

instance Focusable 'ViewMail 'MailAttachmentOpenWithEditor where
  switchFocus _ _ = pure . set (asViews . vsViews . at ViewMail . _Just . vFocus) MailAttachmentOpenWithEditor
                    . set (asViews . vsViews . at ViewMail . _Just . vWidgets . ix MailAttachmentOpenWithEditor . veState) Visible

instance Focusable 'ViewMail 'MailAttachmentPipeToEditor where
  switchFocus _ _ = pure . set (asViews . vsViews . at ViewMail . _Just . vFocus) MailAttachmentPipeToEditor
                    . set (asViews . vsViews . at ViewMail . _Just . vWidgets . ix MailAttachmentPipeToEditor . veState) Visible

instance Focusable 'Help 'ScrollingHelpView where
  switchFocus _ _ = pure . over (asViews . vsFocusedView) (Brick.focusSetCurrent Help)

instance Focusable 'ComposeView 'ComposeListOfAttachments where
  switchFocus _ _ s = pure $ s & set (asViews . vsViews . at ComposeView . _Just . vFocus) ComposeListOfAttachments
                    . resetView Threads indexView

instance Focusable 'ComposeView 'ComposeFrom where
  switchFocus _ _ s = pure $ s & set (asViews . vsViews . at ComposeView . _Just . vFocus) ComposeFrom
                      . set (asCompose . cTemp) (view (asCompose . cTo . E.editContentsL . to currentLine) s)
                      . set (asViews . vsViews . at ComposeView . _Just . vWidgets . ix ComposeFrom . veState) Visible

instance Focusable 'ComposeView 'ComposeTo where
  switchFocus _ _ s = pure $ s & set (asViews . vsViews . at ComposeView . _Just . vFocus) ComposeTo
                      . set (asCompose . cTemp) (view (asCompose . cTo . E.editContentsL . to currentLine) s)
                      . set (asViews . vsViews . at ComposeView . _Just . vWidgets . ix ComposeTo . veState) Visible

instance Focusable 'ComposeView 'ComposeSubject where
  switchFocus _ _ s = pure $ s & set (asViews . vsViews . at ComposeView . _Just . vFocus) ComposeSubject
                      . set (asCompose . cTemp) (view (asCompose . cTo . E.editContentsL . to currentLine) s)
                      . set (asViews . vsViews . at ComposeView . _Just . vWidgets . ix ComposeSubject . veState) Visible

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
    if nullOf (asCompose . cMail) s
        then pure $
             over
                 (asViews . vsFocusedView)
                 (Brick.focusSetCurrent ComposeView)
                 s
        else pure $ s & toggleLastVisibleWidget ComposeFrom
             . over (asCompose . cFrom) (E.applyEdit gotoEOL)

-- | Problem: How to chain actions, which operate not on the same mode, but a
-- mode switched by the previous action?
class HasName (a :: Name) where
  name :: Proxy a -> Name

-- promote the type to a value we can use for chaining actions
instance HasName 'ListOfMails where
  name _ = ListOfMails

instance HasName 'SearchThreadsEditor where
  name _ = SearchThreadsEditor

instance HasName 'ScrollingMailView where
  name _ = ScrollingMailView

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

-- | Allow to change the view to a different view in order to put the focus on a widget there
class ViewTransition (v :: ViewName) (v' :: ViewName) where
  transitionHook :: Proxy v -> Proxy v' -> AppState -> AppState
  transitionHook _ _ = id

instance ViewTransition v v where

instance ViewTransition 'Mails 'Threads where
  transitionHook _ _ = set (asViews . vsViews . ix Mails) listOfMailsView

instance ViewTransition 'Threads 'Mails where

instance ViewTransition 'Threads 'ComposeView where

instance ViewTransition 'Threads 'ViewMail where

instance ViewTransition 'Help v where

instance ViewTransition v 'Help where

instance ViewTransition 'ComposeView 'Threads where

instance ViewTransition 'ComposeView 'FileBrowser where

instance ViewTransition 'Mails 'ViewMail where
  transitionHook _ _ = set (asViews . vsViews . ix ViewMail) mailView

instance ViewTransition 'ViewMail 'Mails where
  transitionHook _ _ = set (asViews . vsViews . ix Mails) listOfMailsView

instance ViewTransition 'ViewMail 'ComposeView where

instance ViewTransition 'FileBrowser 'ComposeView where

instance ViewTransition 'ViewMail 'Threads where


class HasViewName (a :: ViewName) where
  viewname :: Proxy a -> ViewName

instance HasViewName 'Threads where
  viewname _ = Threads

instance HasViewName 'ViewMail where
  viewname _ = ViewMail

instance HasViewName 'Mails where
  viewname _ = Mails

instance HasViewName 'Help where
  viewname _ = Help

instance HasViewName 'ComposeView where
  viewname _ = ComposeView

instance HasViewName 'FileBrowser where
  viewname _ = FileBrowser


quit :: Action v ctx (T.Next AppState)
quit = Action ["quit the application"] Brick.halt

continue :: Action v ctx (T.Next AppState)
continue = Action mempty Brick.continue

invokeEditor :: Action v ctx (T.Next AppState)
invokeEditor = Action ["invoke external editor"] (Brick.suspendAndResume . liftIO . invokeEditor')

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
             . set (asViews . vsViews . at ViewMail . _Just . vWidgets . ix MailAttachmentOpenWithEditor . veState) Visible
  }

openWithCommand :: Action 'ViewMail 'MailAttachmentOpenWithEditor (T.Next AppState)
openWithCommand =
  Action
    { _aDescription = ["ask for command to open attachment"]
    , _aAction =
        \s ->
          let cmd = view (asMailView . mvOpenCommand . E.editContentsL . to (T.unpack . currentLine)) s
           in case cmd of
            [] -> Brick.continue $ setError (GenericError "Empty command") s
            (x:xs) -> Brick.suspendAndResume $ liftIO $ openCommand' s (MailcapHandler (Process (x :| xs) []) False)
    }

pipeToCommand :: Action 'ViewMail 'MailAttachmentPipeToEditor (T.Next AppState)
pipeToCommand =
  Action
  { _aDescription = ["pipe to external command"]
  , _aAction = \s ->
    let cmd = view (asMailView . mvPipeCommand . E.editContentsL . to (T.unpack . currentLine)) s
    in Brick.suspendAndResume $ liftIO $ pipeCommand' s cmd
  }

chain :: Action v ctx AppState -> Action v ctx a -> Action v ctx a
chain (Action d1 f1) (Action d2 f2) = Action (d1 <> d2) (f1 >=> f2)

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

focus :: forall a v. (HasViewName v, HasName a, Focusable v a) => Action v a AppState
focus = Action
  ["switch mode to " <> T.pack (show (name (Proxy :: Proxy a)))]
  (switchFocus (Proxy :: Proxy v) (Proxy :: Proxy a))

-- | A no-op action which just returns the current AppState
-- This action can be used at the start of an Action chain where an immediate
-- mode switch is required
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

setUnread :: Action 'Mails 'ListOfMails AppState
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

switchComposeEditor :: Action 'Threads 'ListOfThreads AppState
switchComposeEditor =
    Action
    { _aDescription = ["switch to compose editor"]
    , _aAction = \s -> if has (asCompose . cAttachments . traversed) s
                          then pure $ over (asViews . vsFocusedView) (Brick.focusSetCurrent ComposeView) s
                          else pure s
    }

replyMail :: Action 'ViewMail 'ScrollingMailView AppState
replyMail =
    Action
    { _aDescription = ["reply to an e-mail"]
    , _aAction = replyToMail
    }

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

setTags :: [TagOp] -> Action v ctx AppState
setTags ops =
    Action
    { _aDescription = ["apply given tags"]
    , _aAction = \s -> case focusedViewWidget s of
          ListOfMails -> selectedItemHelper (asMailIndex . miListOfMails) s (manageMailTags s ops)
          _ -> selectedItemHelper (asMailIndex . miListOfThreads) s (manageThreadTags s ops)
    }

reloadList :: Action 'Threads 'ListOfThreads AppState
reloadList = Action ["reload list of threads"] applySearch

selectNextUnread :: Action 'Mails 'ListOfMails AppState
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

createAttachments :: Action 'FileBrowser 'ListOfFiles AppState
createAttachments =
    Action
        ["adds selected files as attachments"]
        (\s ->
              if isFileUnderCursor $ L.listSelectedElement $ view (asFileBrowser . fbEntries) s
              then liftIO $ makeAttachmentsFromSelected s
              else pure s)

-- Function definitions for actions
--
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

isFileUnderCursor :: Maybe (a, (b, FileSystemEntry)) -> Bool
isFileUnderCursor i = maybe False isFile (preview (_Just . _2 . _2) i)
  where isFile (File _) = True
        isFile _ = False

fullpath :: AppState -> (a, FileSystemEntry) -> FilePath
fullpath s i = currentLine (view (asFileBrowser . fbSearchPath . E.editContentsL) s) </> view (_2 . fsEntryName) i

updateBrowseFileContents :: [FileSystemEntry] -> AppState -> AppState
updateBrowseFileContents contents s =
  let contents' = view vector ((False, ) <$> contents)
  in over (asFileBrowser . fbEntries) (L.listReplace contents' (Just 0)) s

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

setMailsForThread :: AppState -> IO AppState
setMailsForThread s = selectedItemHelper (asMailIndex . miListOfThreads) s $ \t ->
  let dbpath = view (asConfig . confNotmuch . nmDatabase) s
      updateThreadMails vec =
        over (asMailIndex . miMails . listList) (L.listReplace vec Nothing)
        . set (asMailIndex . miMails . listLength) (Just (length vec))
  in either setError updateThreadMails <$> runExceptT (Notmuch.getThreadMessages dbpath t)

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

getEditorTagOps :: HasEditor n => Proxy n -> AppState -> Either Error [TagOp]
getEditorTagOps p s =
  let contents = (foldr (<>) "" $ E.getEditContents $ view (editorL p) s)
  in parseTagOps contents

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
updateStateWithParsedMail s = selectedItemHelper (asMailIndex . miListOfMails) s $ \m ->
        either
            (\e -> setError e . over (asViews . vsFocusedView) (Brick.focusSetCurrent Threads))
            (\pmail -> set (asMailView . mvMail) (Just pmail)
                       . over (asViews . vsFocusedView) (Brick.focusSetCurrent ViewMail)
                       . set (asMailView . mvAttachments) (setEntities pmail)
            )
            <$> runExceptT (parseMail m (view (asConfig . confNotmuch . nmDatabase) s))
  where
    setEntities m = L.list MailListOfAttachments (view vector $ toListOf entities m) 0

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

setError :: Error -> AppState -> AppState
setError = set asError . Just

replyToMail :: AppState -> T.EventM Name AppState
replyToMail s =
  ($ s) <$> case L.listSelectedElement (view (asMailIndex . miListOfMails) s) of
    Just (_, m) ->
      either handleErr handleMail
        . (>>= toQuotedMail charsets preferredContentType)
      <$> runExceptT (parseMail m (view (asConfig . confNotmuch . nmDatabase) s))
    Nothing -> pure id
  where
    charsets = view (asConfig . confCharsets) s
    preferredContentType = view (asConfig . confMailView . mvPreferredContentType) s
    handleErr e = over (asViews . vsFocusedView) (Brick.focusSetCurrent Threads) . setError e
    handleMail pmail s' = s' &
      set (asCompose . cTo) (E.editor ComposeTo Nothing $ getTo pmail)
      . set (asCompose . cFrom) (E.editor ComposeFrom Nothing $ getFrom pmail)
      . set (asCompose . cSubject) (E.editor ComposeSubject Nothing (getSubject pmail))
      . over (asCompose . cAttachments) (upsertPart charsets pmail)

sendMail :: AppState -> T.EventM Name AppState
sendMail s = do
    dateTimeNow <- liftIO getCurrentTime
    let to' = either (pure []) id $ parseOnly addressList $ T.encodeUtf8 $ T.unlines $ E.getEditContents $ view (asCompose . cTo) s
        from = either (pure []) id $ parseOnly mailboxList $ T.encodeUtf8 $ T.unlines $ E.getEditContents $ view (asCompose . cFrom) s
        subject = T.unlines $ E.getEditContents $ view (asCompose . cSubject) s
        attachments' = toListOf (asCompose . cAttachments . L.listElementsL . traversed) s
        (b, l') = splitAt 50 $ view (asConfig . confBoundary) s
        mail = if has (asCompose . cAttachments . L.listElementsL . traversed . filtered isAttachment) s
                then Just $ createMultipartMixedMessage (C8.pack b) attachments'
                else firstOf (asCompose . cAttachments . L.listElementsL . traversed) s
        charsets = view (asConfig . confCharsets) s
    case mail of
        Nothing -> pure $ setError (GenericError "Black hole detected") s
        (Just m) -> let m' = m
                          & set (headers . at "Subject") (Just $ T.encodeUtf8 subject)
                          . set (headers . at "From") (Just $ renderMailboxes from)
                          . set (headers . at "To") (Just $ renderAddresses to')
                          . set (headers . at "Date") (Just $ renderRFC5422Date dateTimeNow)
                    in liftIO $ trySendAndCatch l' (renderMessage $ sanitizeMail charsets m') s

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
  over (attachments . headers . contentDisposition . filename charsets) takeFileName

initialCompose :: [Mailbox] -> Compose
initialCompose mailboxes =
  let mail = B.empty
  in Compose
        mail
        (E.editorText ComposeFrom (Just 1) (AddressText.renderMailboxes mailboxes))
        (E.editorText ComposeTo (Just 1) "")
        (E.editorText ComposeSubject (Just 1) "")
        T.empty
        (L.list ComposeListOfAttachments mempty 1)

-- | Serialise the WireEntity and write it to a temporary file. If no WireEntity
-- exists (e.g. composing a new mail) just use the empty file. When the
-- serialising fails, we return an error. Once the editor exits, read the
-- contents from the temporary file, delete it and create a MIME message out of
-- it. Set it in the Appstate.
invokeEditor' :: AppState -> IO AppState
invokeEditor' s =
  let maybeEntity = preview (asCompose . cAttachments . to L.listSelectedElement
                             . _Just . _2 . to getTextPlainPart . _Just) s
      cmd = view (asConfig . confEditor) s
      updatePart s' tempfile = do
        contents <- tryIO $ T.readFile tempfile
        let mail = createTextPlainMessage contents
        pure $ s' & over (asCompose . cAttachments) (upsertPart charsets mail)
      mkEntity :: (MonadError Error m) => m B.ByteString
      mkEntity = maybe (pure mempty) entityToBytes maybeEntity
      entityCmd = EntityCommand updatePart (tmpfileResource True) (\_ fp -> proc cmd [fp])
      charsets = view (asConfig . confCharsets) s
  in
    either (`setError` s) id
    <$> runExceptT (mkEntity >>= flip runEntityCommand s . entityCmd)

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
      let con = EntityCommand (const . pure) (tmpfileResource (view mhKeepTemp cmd))
            (\_ fp -> toProcessConfigWithTempfile (view mhMakeProcess cmd) fp)
      in fmap con . entityToBytes
  in either (`setError` s) id
      <$> runExceptT (selectedAttachmentOrError s >>= mkConfig >>= flip runEntityCommand s)

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
        let con = EntityCommand (const . pure) emptyResource
              (\b _ -> setStdin (byteStringInput $ LB.fromStrict b) (proc cmd []))
        in fmap con . entityToBytes
     in either (`setError` s) id
        <$> runExceptT (selectedAttachmentOrError s >>= mkConfig >>= flip runEntityCommand s)

selectedAttachmentOrError :: MonadError Error m => AppState -> m WireEntity
selectedAttachmentOrError =
  maybe (throwError $ GenericError "No attachment selected") pure
  . preview (asMailView . mvAttachments . to L.listSelectedElement . _Just . _2)

editAttachment :: AppState -> IO AppState
editAttachment s =
    case L.listSelectedElement $ view (asCompose . cAttachments) s of
        Nothing -> pure $ setError (GenericError "No file selected to edit") s
        Just (_, m) -> case preview (headers . contentDisposition . dispositionType) m of
          (Just Inline) -> invokeEditor' s
          _ -> pure $ setError (GenericError "Not implemented. See #182") s

upsertPart :: CharsetLookup -> MIMEMessage -> L.List Name MIMEMessage -> L.List Name MIMEMessage
upsertPart charsets newPart l =
  case L.listSelectedElement l of
    Nothing -> L.listInsert 0 newPart l
    Just (_, part) ->
      if view (headers . contentDisposition . filename charsets) part
          == view (headers . contentDisposition . filename charsets) newPart
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
