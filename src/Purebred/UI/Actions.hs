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

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

module Purebred.UI.Actions (
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
  , (!*>)
  , done
  , abort
  , noop
  , chain
  , switchView
  , bindAction
  , ifte

  -- ** List specific Actions
  , listUp
  , listDown
  , listJumpToEnd
  , listJumpToStart
  , reloadList
  , toggleListItem
  , untoggleListItems

  -- ** Mail specific Actions
  , displayMail
  , setUnread
  , displayThreadMails
  , toggleHeaders
  , switchComposeEditor
  , senderReply
  , groupReply
  , encapsulateMail
  , selectNextUnread
  , composeAsNew
  , createAttachments
  , openAttachment
  , setTags
  , saveAttachmentToPath
  , searchRelated

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
  , handleConfirm
  , fileBrowserToggleFile

  -- ** Debug actions
  , debug

  -- * API
  , applySearch
  , initialCompose
  ) where

import qualified Brick
import Brick.BChan (writeBChan)
import qualified Brick.Focus as Brick
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.FileBrowser as FB
import Brick.Widgets.Dialog (dialog, dialogSelection, Dialog)
import Network.Mime (defaultMimeLookup)
import Data.Kind (Type)
import Data.Proxy
import Data.Either (fromRight, isRight)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import qualified Data.Attoparsec.Text as AT (parseOnly)
import Data.Vector.Lens (vector)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List (union)
import qualified Data.Vector as Vector
import Prelude hiding (readFile, unlines)
import Data.Foldable (fold, toList, traverse_)
import Data.Functor.Identity (Identity(..))
import Control.Lens
       (_Just, to, at, ix, _1, _2, toListOf, traversed, has,
        filtered, set, over, preview, view, views, (&), firstOf, non, Traversal',
        Getting, Lens', folded, assign, modifying, preuse, use, uses
        , Ixed, Index, IxValue)
import Control.Concurrent (forkIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State
import Control.Monad.Catch (MonadCatch, MonadMask, catch)
import Control.Monad.Except (runExceptT, MonadError, throwError)
import Control.Exception (IOException)
import Data.Text.Zipper
       (insertMany, currentLine, gotoEOL, clearZipper)
import Data.Time (getCurrentTime, getZonedTime)
import System.Directory (doesPathExist)
import System.Random (getStdRandom, uniform)

import qualified Data.IMF.Text as AddressText
import Data.MIME
import qualified Purebred.Storage.Notmuch as Notmuch
import Purebred.Storage.Mail
       ( parseMail, toQuotedMail
       , entityToBytes, toMIMEMessage, takeFileName, bodyToDisplay
       , removeMatchingWords, findMatchingWords, makeScrollSteps
       , writeEntityToPath)
import Types
import Purebred.UI.Views
       (mailView, toggleLastVisibleWidget, indexView, resetView,
        focusedViewWidget)
import Purebred.Events (nextGeneration)
import Purebred.Plugin.Internal
import Purebred.Tags (parseTagOps)
import Purebred.Types.Error
import Purebred.System (tryIO)
import Purebred.System.Process
import Purebred.UI.Notifications
       (setUserMessage, makeWarning, showError, showWarning, showInfo
       , showUserMessage)
import Brick.Widgets.StatefulEdit
       (StatefulEditor(..), editEditorL, revertEditorState, saveEditorState)
#if defined LAZYVECTOR
import Purebred.LazyVector (V)
#endif



{- $overview

Actions are composible functions. They can be sequenced and used in 'Keybinding's.

-}

{- $examples

This keybinding registered to backspace, scrolls a page up and
continues with the event loop:

@
'Keybinding' (EvKey KBS []) ('scrollPageUp' ``chain`` 'continue')
@

This keybinding is used to change the view and focus to a different widget:

@
'Keybinding' (EvKey KEsc []) ('noop' ``focus`` 'continue' @@''Threads' @@''ListOfThreads')
@
-}

{- $new_actions
New 'Action's are typically added when creating a 'Keybinding'
first. Keep the 'Action' specific to the view and widget
(e.g. 'displayMail', or 'displayThreadMails'). You can access the full
'AppState' in the Action's function including 'IO' (see
'createAttachments' as an example).
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
  editorL _ = asCompose . cFrom . editEditorL

instance HasEditor 'ComposeTo where
  editorL _ = asCompose . cTo . editEditorL

instance HasEditor 'ComposeCc where
  editorL _ = asCompose . cCc . editEditorL

instance HasEditor 'ComposeBcc where
  editorL _ = asCompose . cBcc . editEditorL

instance HasEditor 'ComposeSubject where
  editorL _ = asCompose . cSubject . editEditorL

instance HasEditor 'ManageMailTagsEditor where
  editorL _ = asThreadsView . miMailTagsEditor

instance HasEditor 'MailAttachmentOpenWithEditor where
  editorL _ = asMailView . mvOpenCommand

instance HasEditor 'MailAttachmentPipeToEditor where
  editorL _ = asMailView . mvPipeCommand

instance HasEditor 'ScrollingMailViewFindWordEditor where
  editorL _ = asMailView . mvFindWordEditor

instance HasEditor 'SearchThreadsEditor where
  editorL _ = asThreadsView . miSearchThreadsEditor . editEditorL

instance HasEditor 'ManageThreadTagsEditor where
  editorL _ = asThreadsView . miThreadTagsEditor

instance HasEditor 'SaveToDiskPathEditor where
  editorL _ = asMailView . mvSaveToDiskPath

-- | Contexts that have a navigable (Brick) list
class HasList (n :: Name) where
  type T n :: Type -> Type
  type E n
  list :: Proxy n -> Lens' AppState (L.GenericList Name (T n) (E n))

instance HasList 'ListOfThreads where
#if defined LAZYVECTOR
  type T 'ListOfThreads = V
#else
  type T 'ListOfThreads = Vector.Vector
#endif
  type E 'ListOfThreads = Toggleable NotmuchThread
  list _ = asThreadsView . miListOfThreads

instance HasList 'ListOfMails where
  type T 'ListOfMails = Vector.Vector
  type E 'ListOfMails = Toggleable NotmuchMail
  list _ = asThreadsView . miListOfMails

instance HasList 'ScrollingMailView where
  type T 'ScrollingMailView = Vector.Vector
  type E 'ScrollingMailView = Toggleable NotmuchMail
  list _ = asThreadsView . miListOfMails

instance HasList 'ComposeListOfAttachments where
  type T 'ComposeListOfAttachments = Vector.Vector
  type E 'ComposeListOfAttachments = MIMEMessage
  list _ = asCompose . cAttachments

instance HasList 'MailListOfAttachments where
  type T 'MailListOfAttachments = Vector.Vector
  type E 'MailListOfAttachments = WireEntity
  list _ = asMailView . mvAttachments

-- | contexts which have selectable items in a list
class (HasList (n :: Name), Traversable (T n)) =>
      HasToggleableList n
  where
  untoggleE :: Proxy n -> E n -> E n
  toggleE :: Proxy n -> E n -> E n
  isToggledE :: Proxy n -> E n -> Bool
  untoggleAll :: (MonadState AppState m) => Proxy n -> m ()
  untoggleAll proxy = modifying (list proxy . traversed) (untoggleE proxy)
  toggle :: Proxy n -> Int -> StateT AppState (T.EventM Name) ()
  -- | Traversal of selected items.  NOT a valid Traversal unless
  -- selected state is preserved
  toggledItemsL :: Proxy n -> Traversal' AppState (E n)
  toggledItemsL proxy = list proxy . traversed . filtered (isToggledE proxy)

instance
  ( HasList n
  , Traversable (T n)
  , E n ~ (Bool, a)
  , Index (T n (Bool, a)) ~ Int
  , IxValue (T n (Bool, a)) ~ (Bool, a)
  , Ixed (T n (Bool, a))
  ) => HasToggleableList n where
  untoggleE _ = set _1 False
  toggleE _ = over _1 not
  isToggledE _ = fst
  toggle proxy i = modifying (list proxy . L.listElementsL . ix i) (toggleE proxy)

-- | A function which is run at the end of a chained sequence of actions.
--
-- For example: the user changes the notmuch search terms to find a
-- particular mail. To apply his changes, he 'completes' his typed in
-- text by pressing Enter.
--
class Completable (n :: Name) where
  type CompletableResult n
  type CompletableResult n = ()
  complete :: (MonadIO m, MonadMask m, MonadState AppState m) => Proxy n -> m (CompletableResult n)

instance Completable 'SearchThreadsEditor where
  complete _ = applySearch

instance Completable 'ManageMailTagsEditor where
  complete _ = do
    get >>= liftIO . completeMailTags >>= put
    hide ViewMail 0 ManageMailTagsEditor
    modifying (asThreadsView . miMailTagsEditor . E.editContentsL) clearZipper

instance Completable 'ComposeListOfAttachments where
  type CompletableResult 'ComposeListOfAttachments = Bool
  complete _ = sendMail

-- | Apply all given tag operations to existing mails
--
completeMailTags :: AppState -> IO AppState
completeMailTags s =
    case getEditorTagOps (Proxy @'ManageMailTagsEditor) s of
        Left msg -> pure $ set asUserMessage (Just msg) s
        Right ops -> flip execStateT s $ do
            modifying (asThreadsView . miListOfThreads) (L.listModify (over _2 $ Notmuch.tagItem ops))
            toggledOrSelectedItemHelper
              (Proxy @'ScrollingMailView)
              (manageMailTags ops)
              (over _2 (Notmuch.tagItem ops))
            modify (toggleLastVisibleWidget ManageMailTagsEditor)

hide, unhide :: (MonadState AppState m) => ViewName -> Int -> Name -> m ()
hide = setViewState Hidden
unhide = setViewState Visible

setViewState :: (MonadState AppState m) => ViewState -> ViewName -> Int -> Name -> m ()
setViewState v n i m =
  assign (asViews . vsViews . ix n . vLayers . ix i . ix m . veState) v

instance Completable 'ComposeTo where
  complete _ = do
    hide ComposeView 1 ComposeTo
    hide ViewMail 0 ComposeTo

instance Completable 'ComposeCc where
  complete _ = do
    hide ComposeView 1 ComposeCc
    hide ViewMail 0 ComposeCc

instance Completable 'ComposeBcc where
  complete _ = do
    hide ComposeView 1 ComposeBcc
    hide ViewMail 0 ComposeBcc

instance Completable 'ComposeFrom where
  complete _ = hide ComposeView 1 ComposeFrom

instance Completable 'ComposeSubject where
  complete _ = hide ComposeView 1 ComposeSubject

instance Completable 'ConfirmDialog where
  complete _ = hide ComposeView 0 ConfirmDialog

-- | Applying tag operations on threads
-- Note: notmuch does not support adding tags to threads themselves, instead we'll
-- apply all tag operations on mails in the thread. Instead of reloading the
-- thread, we'll apply all tag operations on the thread type as well, which are
-- not persisted to the database. This strategy is faster since it does not need
-- any database access above tagging mails, but it could pose a problem if tags
-- don't show up in the UI.
--
instance Completable 'ManageThreadTagsEditor where
  complete _ = do
    s <- get
    case getEditorTagOps (Proxy @'ManageThreadTagsEditor) s of
      Left msg -> showUserMessage msg
      Right ops -> do
        toggledOrSelectedItemHelper
          (Proxy @'ListOfThreads)
          (manageThreadTags ops)
          (over _2 (Notmuch.tagItem ops))
        modify (toggleLastVisibleWidget SearchThreadsEditor)

instance Completable 'ManageFileBrowserSearchPath where
  complete _ = fileBrowserSetWorkingDirectory

instance Completable 'MailAttachmentOpenWithEditor where
  complete _ = hide ViewMail 0 MailAttachmentOpenWithEditor

instance Completable 'MailAttachmentPipeToEditor where
  complete _ = hide ViewMail 0 MailAttachmentPipeToEditor

instance Completable 'SaveToDiskPathEditor where
  complete _ = hide ViewMail 0 SaveToDiskPathEditor

instance Completable 'ScrollingMailViewFindWordEditor where
  complete _ = do
    needle <- uses (asMailView . mvFindWordEditor . E.editContentsL) currentLine
    bod <- uses (asMailView . mvBody) (findMatchingWords needle)
    hide ViewMail 0 ScrollingMailViewFindWordEditor
    assign (asMailView . mvScrollSteps) (Brick.focusRing (makeScrollSteps bod))
    assign (asMailView . mvBody) bod

-- | Generalisation of reset actions, whether they reset editors back to their
-- initial state or throw away composed, but not yet sent mails.
--
class Resetable (v :: ViewName) (n :: Name) where
  reset :: (MonadIO m, MonadState AppState m) => Proxy v -> Proxy n -> m ()

instance Resetable 'Threads 'SearchThreadsEditor where
  reset _ _ = modifying (asThreadsView . miSearchThreadsEditor) revertEditorState

instance Resetable 'ViewMail 'ManageMailTagsEditor where
  reset _ _ = modifying (asThreadsView . miMailTagsEditor . E.editContentsL) clearZipper
              *> hide ViewMail 0 ManageMailTagsEditor

instance Resetable 'Threads 'ManageThreadTagsEditor where
  reset _ _ = do
    modifying (asThreadsView . miThreadTagsEditor . E.editContentsL) clearZipper
    modify (toggleLastVisibleWidget SearchThreadsEditor)

instance Resetable 'Threads 'ComposeFrom where
  reset _ _ = modify clearMailComposition

instance Resetable 'Threads 'ComposeSubject where
  reset _ _ = modify clearMailComposition

instance Resetable 'Threads 'ComposeTo where
  reset _ _ = modify clearMailComposition

instance Resetable 'ComposeView 'ComposeFrom where
  reset _ _ = do
    modifying (asCompose . cFrom) revertEditorState
    hide ComposeView 1 ComposeFrom

instance Resetable 'ComposeView 'ComposeTo where
  reset _ _ = do
    modifying (asCompose . cTo) revertEditorState
    hide ComposeView 1 ComposeTo

instance Resetable 'ComposeView 'ComposeCc where
  reset _ _ = do
    modifying (asCompose . cCc) revertEditorState
    hide ComposeView 1 ComposeCc

instance Resetable 'ComposeView 'ComposeBcc where
  reset _ _ = do
    modifying (asCompose . cBcc) revertEditorState
    hide ComposeView 1 ComposeBcc

instance Resetable 'ComposeView 'ComposeSubject where
  reset _ _ = do
    modifying (asCompose . cSubject) revertEditorState
    hide ComposeView 1 ComposeSubject

instance Resetable 'ComposeView 'ComposeListOfAttachments where
  reset _ _ = modify clearMailComposition

instance Resetable 'FileBrowser 'ManageFileBrowserSearchPath where
  reset _ _ = modifying (asFileBrowser . fbSearchPath) revertEditorState

instance Resetable 'ViewMail 'MailListOfAttachments where
  reset _ _ = hide ViewMail 0 MailListOfAttachments

instance Resetable 'ViewMail 'MailAttachmentOpenWithEditor where
  reset _ _ = do
    modifying (asMailView . mvOpenCommand . E.editContentsL) clearZipper
    hide ViewMail 0 MailAttachmentOpenWithEditor

instance Resetable 'ViewMail 'MailAttachmentPipeToEditor where
  reset _ _ = do
    modifying (asMailView . mvPipeCommand . E.editContentsL) clearZipper
    hide ViewMail 0 MailAttachmentPipeToEditor

instance Resetable 'ViewMail 'ScrollingMailViewFindWordEditor where
  reset _ _ = do
    modifying (asMailView . mvFindWordEditor . E.editContentsL) clearZipper
    hide ViewMail 0 ScrollingMailViewFindWordEditor
    modify resetMatchingWords

instance Resetable 'ViewMail 'ScrollingMailView where
  reset _ _ = modify resetMatchingWords

instance Resetable 'ViewMail 'SaveToDiskPathEditor where
  reset _ _ = do
    modifying (asMailView . mvSaveToDiskPath . E.editContentsL) clearZipper
    hide ViewMail 0 SaveToDiskPathEditor

instance Resetable 'ViewMail 'ComposeTo where
  reset _ _ = do
    modifying (asCompose . cTo) revertEditorState
    hide ViewMail 0 ComposeTo
    modify clearMailComposition

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
class Focusable (v :: ViewName) (n :: Name) where
  onFocusSwitch :: (MonadState AppState m, MonadIO m) => Proxy v -> Proxy n -> m ()

instance Focusable 'Threads 'SearchThreadsEditor where
  onFocusSwitch _ _ = do
    modifying (asThreadsView . miSearchThreadsEditor . editEditorL) (E.applyEdit gotoEOL)
    modifying (asThreadsView . miSearchThreadsEditor) saveEditorState

instance Focusable 'Threads 'ManageThreadTagsEditor where
  onFocusSwitch _ _ = do
    modifying (asThreadsView . miThreadTagsEditor . E.editContentsL) clearZipper
    modify (toggleLastVisibleWidget ManageThreadTagsEditor)

instance Focusable 'Threads 'ComposeFrom where
  onFocusSwitch _ _ = do
    modify (toggleLastVisibleWidget ComposeFrom)
    modifying (asCompose . cFrom . editEditorL) (E.applyEdit gotoEOL)

instance Focusable 'Threads 'ComposeTo where
  onFocusSwitch _ _ = modify (toggleLastVisibleWidget ComposeTo)

instance Focusable 'Threads 'ComposeSubject where
  onFocusSwitch _ _ = modify (toggleLastVisibleWidget ComposeSubject)

instance Focusable 'Threads 'ListOfThreads where
  onFocusSwitch _ _ = pure ()

instance Focusable 'ViewMail 'ManageMailTagsEditor where
  onFocusSwitch _ _ = do
    modifying (asThreadsView . miMailTagsEditor . E.editContentsL) clearZipper
    unhide ViewMail 0 ManageMailTagsEditor
    assign (asViews . vsViews . ix ViewMail . vFocus) ManageMailTagsEditor

instance Focusable 'ViewMail 'ScrollingMailView where
  onFocusSwitch _ _ = assign (asViews . vsViews . ix ViewMail . vFocus) ScrollingMailView

instance Focusable 'ViewMail 'ScrollingMailViewFindWordEditor where
  onFocusSwitch _ _ = do
    modifying (asMailView . mvFindWordEditor . E.editContentsL) clearZipper
    assign (asViews. vsViews . ix ViewMail . vFocus) ScrollingMailViewFindWordEditor
    unhide ViewMail 0 ScrollingMailViewFindWordEditor

instance Focusable 'ViewMail 'ListOfMails where
  onFocusSwitch _ _ = assign (asViews . vsViews . ix ViewMail . vFocus) ListOfMails

instance Focusable 'ViewMail 'MailListOfAttachments where
  onFocusSwitch _ _ = do
    assign (asViews . vsViews . ix ViewMail . vFocus) MailListOfAttachments
    unhide ViewMail 0 MailListOfAttachments

instance Focusable 'ViewMail 'MailAttachmentOpenWithEditor where
  onFocusSwitch _ _ = do
    assign (asViews . vsViews . ix ViewMail . vFocus) MailAttachmentOpenWithEditor
    unhide ViewMail 0 MailAttachmentOpenWithEditor

instance Focusable 'ViewMail 'MailAttachmentPipeToEditor where
  onFocusSwitch _ _ = do
    assign (asViews . vsViews . ix ViewMail . vFocus) MailAttachmentPipeToEditor
    unhide ViewMail 0 MailAttachmentPipeToEditor

instance Focusable 'ViewMail 'SaveToDiskPathEditor where
  onFocusSwitch _ _ = do
    charsets <- use (asConfig . confCharsets)
    s <- get
    let maybeFilePath = preview (asMailView . mvAttachments . to L.listSelectedElement
                                 . _Just . _2 . contentDisposition . folded . filename charsets) s
        fname = view (non mempty) maybeFilePath
    assign (asViews . vsViews . ix ViewMail . vFocus) SaveToDiskPathEditor
    unhide ViewMail 0 SaveToDiskPathEditor
    modifying (asMailView . mvSaveToDiskPath . E.editContentsL) (insertMany fname . clearZipper)

instance Focusable 'ViewMail 'ComposeTo where
  onFocusSwitch _ _ = do
    assign (asViews . vsViews . ix ViewMail . vFocus) ComposeTo
    unhide ViewMail 0 ComposeTo

instance Focusable 'Help 'ScrollingHelpView where
  onFocusSwitch _ _ = modifying (asViews . vsFocusedView) (Brick.focusSetCurrent Help)

instance Focusable 'ComposeView 'ComposeListOfAttachments where
  onFocusSwitch _ _ = do
    assign (asViews . vsViews . ix ComposeView . vFocus) ComposeListOfAttachments
    modify (resetView Threads indexView)

instance Focusable 'ComposeView 'ComposeFrom where
  onFocusSwitch _ _ = do
    assign (asViews . vsViews . ix ComposeView . vFocus) ComposeFrom
    modifying (asCompose . cFrom) saveEditorState
    unhide ComposeView 1 ComposeFrom

instance Focusable 'ComposeView 'ComposeTo where
  onFocusSwitch _ _ = do
    assign (asViews . vsViews . ix ComposeView . vFocus) ComposeTo
    modifying (asCompose . cTo) saveEditorState
    unhide ComposeView 1 ComposeTo

instance Focusable 'ComposeView 'ComposeCc where
  onFocusSwitch _ _ = do
    assign (asViews . vsViews . ix ComposeView . vFocus) ComposeCc
    modifying (asCompose . cCc) saveEditorState
    unhide ComposeView 1 ComposeCc

instance Focusable 'ComposeView 'ComposeBcc where
  onFocusSwitch _ _ = do
    assign (asViews . vsViews . ix ComposeView . vFocus) ComposeBcc
    modifying (asCompose . cBcc) saveEditorState
    unhide ComposeView 1 ComposeBcc

instance Focusable 'ComposeView 'ComposeSubject where
  onFocusSwitch _ _ = do
    assign (asViews . vsViews . ix ComposeView . vFocus) ComposeSubject
    modifying (asCompose . cSubject) saveEditorState
    unhide ComposeView 1 ComposeSubject

instance Focusable 'ComposeView 'ConfirmDialog where
  onFocusSwitch _ _ = do
    assign (asViews . vsViews . ix ComposeView . vFocus) ConfirmDialog
    unhide ComposeView 0 ConfirmDialog

instance Focusable 'FileBrowser 'ListOfFiles where
  onFocusSwitch _ _ = fileBrowserSetWorkingDirectory

instance Focusable 'FileBrowser 'ManageFileBrowserSearchPath where
  onFocusSwitch _ _ = pure ()


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

instance HasName 'ComposeCc where
  name _ = ComposeCc

instance HasName 'ComposeBcc where
  name _ = ComposeBcc

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

instance HasName 'SaveToDiskPathEditor where
  name _ = SaveToDiskPathEditor

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
quit = Action ["quit the application"] (get >>= lift . Brick.halt)

-- | A noop used to continue the Brick event loop.
--
continue :: Action v ctx (T.Next AppState)
continue = Action mempty (get >>= lift . Brick.continue)

-- | Suspends Purebred and invokes the configured editor.
--
invokeEditor :: ViewName -> Name -> Action v ctx (T.Next AppState)
invokeEditor n w =
  Action
    ["invoke external editor"]
    (let updatePart t =
           modifying
             (asCompose . cAttachments)
             (insertOrReplaceAttachment $ createTextPlainMessage t)
         errormsg e =
           (ProcessError $
           "Editor exited abnormally ( " <>
           show e <> " ). Press Esc to continue.")
      in stateSuspendAndResume $
         runExceptT invokeEditor' >>=
         either (\e -> showError (errormsg e) *> switchMode' n w) updatePart)

-- | Suspends Purebred to invoke a command for editing an
-- attachment. Currently only supports re-editing the body text of an
-- e-mail.
--
edit :: Action 'ComposeView 'ComposeListOfAttachments (T.Next AppState)
edit =
  Action
    ["edit file"]
    (stateSuspendAndResume (editAttachment ComposeView ComposeListOfAttachments))

openAttachment :: Action 'ViewMail ctx (T.Next AppState)
openAttachment =
  Action
  { _aDescription = ["open attachment with external command"]
  , _aAction = do
      s <- get
      let
        match ct = firstOf (asConfig . confMailView . mvMailcap . traversed
                            . filtered (`fst` ct)
                            . _2) s
        maybeCommand =
          match
          =<< preview (asMailView . mvAttachments . to L.listSelectedElement . _Just . _2 . headers . contentType) s
      case maybeCommand of
        Just cmd -> openCommand' cmd *> get >>= (lift . Brick.continue)
        Nothing -> do
          let l = asViews . vsViews . ix ViewMail
          assign (l . vFocus) MailAttachmentOpenWithEditor
          assign (l . vLayers . ix 0 . ix MailAttachmentOpenWithEditor . veState) Visible
          lift . Brick.continue =<< get
  }

-- | Open the selected entity with the command given from the editor widget.
--
openWithCommand :: Action 'ViewMail 'MailAttachmentOpenWithEditor (T.Next AppState)
openWithCommand =
  Action
    { _aDescription = ["ask for command to open attachment"]
    , _aAction = do
      cmd <- uses (asMailView . mvOpenCommand . E.editContentsL) (T.unpack . currentLine)
      case cmd of
        [] -> lift . Brick.continue . setUserMessage (makeWarning StatusBar "Empty command") =<< get
        (x:xs) -> stateSuspendAndResume $
          openCommand' (MailcapHandler (Process (x :| xs) []) IgnoreOutput KeepTempfile)
    }

-- | Wrapper for 'Brick.suspendAndResume' that reads state from
-- state monad.  The resulting (post-resume) state is returned, but
-- it is NOT set as the new state.  (To do this would, I think,
-- require some MVar trickery, because we can't get at the value
-- in the 'T.Next AppState' returned by 'Brick.suspendAndResume'.
-- It is feasible, but we don't have a use case yet.)
stateSuspendAndResume :: StateT AppState IO a -> StateT AppState (T.EventM n) (T.Next AppState)
stateSuspendAndResume go = lift . Brick.suspendAndResume . execStateT (go *> get) =<< get

-- | Pipe the selected entity to the command given from the editor widget.
--
pipeToCommand :: Action 'ViewMail 'MailAttachmentPipeToEditor (T.Next AppState)
pipeToCommand =
  Action
  { _aDescription = ["pipe to external command"]
  , _aAction = do
      cmd <- uses (asMailView . mvPipeCommand . E.editContentsL) (T.unpack . currentLine)
      stateSuspendAndResume (pipeCommand' cmd)
  }

-- | Save a currently selected attachment to the given path on
-- disk. Shows an error if the path is invalid or can not be written
-- to.
saveAttachmentToPath :: Action 'ViewMail 'SaveToDiskPathEditor ()
saveAttachmentToPath =
  Action
  { _aDescription = ["save attachment to disk"]
  , _aAction =
      selectedItemHelper (asMailView . mvAttachments) $ \ent -> do
        filePath <- uses (asMailView . mvSaveToDiskPath . E.editContentsL) (T.unpack . currentLine)
        runExceptT (writeEntityToPath filePath ent)
          >>= either
            (\e -> do
              showError e
              modifying (asMailView . mvSaveToDiskPath . E.editContentsL) clearZipper )
            (\fp -> showInfo ("Attachment saved to: " <> T.pack fp)
            )
  }

-- | Chain sequences of actions to create a keybinding
--
chain :: Action v ctx a -> Action v ctx b -> Action v ctx b
chain = (*>)

-- | /Special/ form of chain allowing to sequencing actions registered
-- for a different view/widget. This is useful to perform actions on
-- widget focus changes.
--
focus, (!*>) ::
     forall v v' ctx ctx' a b.
     ( Focusable v' ctx'
     , HasName ctx
     , HasViewName v
     , HasName ctx'
     , HasViewName v'
     , ViewTransition v v'
     )
  => Action v ctx a
  -> Action v' ctx' b
  -> Action v ctx b
focus a1 (Action d2 f2) = a1 *> switchView @v' @ctx' *> Action d2 f2
(!*>) = focus

-- | Focus a different view/widget, as specified by type arguments.
--
-- > switchView @'Threads @'ListOfThreads
--
switchView
  :: forall v' ctx' v ctx.
      ( Focusable v' ctx'
      , HasName ctx, HasName ctx'
      , HasViewName v, HasViewName v'
      , ViewTransition v v'
      )
  => Action v ctx ()
switchView = Action [desc] $ do
  sink <- use (asConfig . confLogSink)
  liftIO . sink . LT.pack $ msg
  onFocusSwitch (Proxy @v') (Proxy @ctx')
  modify (transitionHook (Proxy @v) (Proxy @v'))
  modifying (asViews . vsFocusedView) (Brick.focusSetCurrent (viewname (Proxy @v')))
  assign (asViews . vsViews . at (viewname (Proxy @v')) . _Just . vFocus) (name (Proxy @ctx'))
  where
    cur = show (viewname (Proxy @v)) <> "/" <> show (name (Proxy @ctx))
    next = show (viewname (Proxy @v')) <> "/" <> show (name (Proxy @ctx'))
    msg = "focus switch: " <> cur <> " -> " <> next
    desc = T.pack $ "focus " <> next

-- | Log a debug message
debug :: LT.Text -> Action v ctx ()
debug msg = Action [] $ do
  sink <- use (asConfig . confLogSink)
  liftIO $ sink msg

done :: forall a v. (HasViewName v, Completable a) => Action v a (CompletableResult a)
done = Action ["apply"] (complete (Proxy @a))

-- | Like '(>>=)', but 'Action' does not have a lawful 'Monad' instance.
bindAction :: Action view ctx a -> (a -> Action view ctx b) -> Action view ctx b
bindAction (Action desc go1) f = Action desc $ do
  a <- go1
  let Action _desc go2 = f a
  go2

-- | "If-then-else" action combinator.
-- Updates the description with info about the choice.
ifte
  :: Action view ctx Bool
  -> Action view ctx c
  -> Action view ctx c
  -> Action view ctx c
ifte a@(Action aDesc _) t@(Action tDesc _) f@(Action fDesc _) =
  updateDesc (a `bindAction` \b -> if b then t else f)
  where
  updateDesc (Action _ go) = Action newDesc go
  newDesc = aDesc <> ["(" <> T.pack (show tDesc) <> " OR " <> T.pack (show fDesc) <> ")"]

abort :: forall a v. (HasViewName v, Resetable v a) => Action v a ()
abort = Action ["cancel"] (reset (Proxy @v) (Proxy @a))

-- $keybinding_actions
-- These actions are used to sequence other actions together. Think of
-- it like glue functions.
--

-- | A no-op action can
-- be used at the start of a sequence with an immediate switch of
-- focus to a different widget (see 'focus').
--
noop :: Action v ctx ()
noop = Action mempty (pure ())

scrollUp :: forall ctx v. (Scrollable ctx) => Action v ctx ()
scrollUp = Action
  { _aDescription = ["scroll up"]
  , _aAction = lift (Brick.vScrollBy (makeViewportScroller (Proxy @ctx)) (-1))
  }

scrollDown :: forall ctx v. (Scrollable ctx) => Action v ctx ()
scrollDown = Action
  { _aDescription = ["scroll down"]
  , _aAction = lift (Brick.vScrollBy (makeViewportScroller (Proxy @ctx)) 1)
  }

scrollPageUp :: forall ctx v. (Scrollable ctx) => Action v ctx ()
scrollPageUp = Action
  { _aDescription = ["page up"]
  , _aAction = lift (Brick.vScrollPage (makeViewportScroller (Proxy @ctx)) T.Up)
  }

scrollPageDown :: forall ctx v. (Scrollable ctx) => Action v ctx ()
scrollPageDown = Action
  { _aDescription = ["page down"]
  , _aAction = lift (Brick.vScrollPage (makeViewportScroller (Proxy @ctx)) T.Down)
  }

scrollNextWord :: forall ctx v. (Scrollable ctx) => Action v ctx ()
scrollNextWord =
  Action
    { _aDescription = ["find next word in mail body"]
    , _aAction = do
        lift $ Brick.vScrollToBeginning (makeViewportScroller (Proxy @ctx))
        b <- gets (has (asMailView . mvScrollSteps))
        if b
          then do
            modifying (asMailView . mvScrollSteps) Brick.focusNext
            nextLine <- preuse (asMailView . mvScrollSteps . to Brick.focusGetCurrent . _Just . _1)
            let scrollBy = view (non 0) nextLine
            lift $ Brick.vScrollBy (makeViewportScroller (Proxy @ctx)) scrollBy
          else
            showWarning "No match"
    }

-- | Removes any highlighting done by searching in the body text
--
removeHighlights :: Action 'ViewMail 'ScrollingMailView ()
removeHighlights =
  Action
    { _aDescription = ["remove search results highlights"]
    , _aAction = modify resetMatchingWords
    }

displayMail :: Action 'ViewMail 'ScrollingMailView ()
displayMail =
    Action
    { _aDescription = ["display an e-mail"]
    , _aAction = do
        lift $ Brick.vScrollToBeginning (makeViewportScroller (Proxy @'ScrollingMailView))
        updateStateWithParsedMail
        updateReadState RemoveTag
    }

-- | Sets the mail list to the mails for the selected thread. Does
-- not select a mail; a movement action such as 'displayNextUnread'
-- should follow this action.
--
displayThreadMails :: Action 'Threads 'ListOfThreads ()
displayThreadMails =
    Action
    { _aDescription = ["display an e-mail for threads"]
    , _aAction = do
        -- Update the Application state with all mails found for the
        -- currently selected thread.
        dbpath <- use (asConfig . confNotmuch . nmDatabase)
        selectedItemHelper (asThreadsView . miListOfThreads) $ \(_, t) ->
          runExceptT (Notmuch.getThreadMessages dbpath (Identity t))
            >>= either showError (\vec -> do
              modifying (asThreadsView . miMails . listList) (L.listReplace vec Nothing)
              assign (asThreadsView . miMails . listLength) (Just (length vec)) )
    }

setUnread :: Action 'ViewMail 'ScrollingMailView ()
setUnread =
    Action
    { _aDescription = ["toggle unread"]
    , _aAction = updateReadState AddTag
    }

listUp
  :: forall v ctx.  (HasList ctx, Foldable (T ctx), L.Splittable (T ctx))
  => Action v ctx ()
listUp = Action ["list up"] (modifying (list (Proxy @ctx)) L.listMoveUp)

listDown
  :: forall v ctx.  (HasList ctx, Foldable (T ctx), L.Splittable (T ctx))
  => Action v ctx ()
listDown = Action ["list down"] (modifying (list (Proxy @ctx)) L.listMoveDown)

listJumpToStart
  :: forall v ctx.  (HasList ctx, Foldable (T ctx), L.Splittable (T ctx))
  => Action v ctx ()
listJumpToStart = Action ["list top"] (modifying (list (Proxy @ctx)) (L.listMoveTo 0))

listJumpToEnd
  :: forall v ctx.  (HasList ctx, Foldable (T ctx), L.Splittable (T ctx))
  => Action v ctx ()
listJumpToEnd = Action ["list bottom"] (modifying (list (Proxy @ctx)) (L.listMoveTo (-1)))

-- | Action used to either start a composition of a new mail or switch
-- the view to the composition editor if we've already been editing a new
-- mail. The use case here is to continue editing an e-mail while
-- still having the ability to browse existing e-mails.
--
switchComposeEditor :: Action 'Threads 'ListOfThreads ()
switchComposeEditor =
    Action
    { _aDescription = ["switch to compose editor"]
    , _aAction = do
        l <- use (asCompose . cAttachments)
        unless (null l) $
          modifying (asViews . vsFocusedView) (Brick.focusSetCurrent ComposeView)
    }

-- | Update the AppState with a 'MIMEMessage'. The instance will have
-- the current selected 'MIMEMessage' encapsulated as an @inline@
-- message.
encapsulateMail :: Action 'ViewMail 'ScrollingMailView ()
encapsulateMail =
  Action
    { _aDescription = ["forward selected e-mail"]
    , _aAction = do
        mail <- use (asMailView . mvMail)
        case mail of
          Nothing -> showWarning "No mail selected for forwarding"
          Just m -> do
            charsets <- use (asConfig . confCharsets)
            let
              origSubj = views (headerSubject charsets) fold m
              origFrom = views (headerFrom charsets) AddressText.renderAddresses m
              newSubj = "[" <> origFrom <> ": " <> origSubj <> "]"
            modifying (asCompose . cAttachments)
              (L.listInsert 1 (encapsulate m) . L.listInsert 0 (createTextPlainMessage mempty))
            modifying (asCompose . cSubject . editEditorL . E.editContentsL)
              (insertMany newSubj . clearZipper)
    }

-- | Update the 'AppState' with a quoted version of the currently
-- selected mail in order to reply to it.
--
senderReply, groupReply :: Action 'ViewMail 'ScrollingMailView ()
senderReply = Action ["reply"] (replyWithMode ReplyToSender)
groupReply = Action ["group-reply"] (replyWithMode ReplyToGroup)

replyWithMode :: ReplyMode -> StateT AppState (T.EventM Name) ()
replyWithMode mode = do
      mail <- use (asMailView . mvMail)
      charsets <- use (asConfig . confCharsets)
      case mail of
        Nothing -> do
          modifying (asViews . vsFocusedView) (Brick.focusSetCurrent Threads)
          showWarning "No mail selected for replying"
        Just m -> do
          mailboxes <- use (asConfig . confComposeView . cvIdentities)
          let
            idents = case mailboxes of
              [] -> pure $ Mailbox Nothing (AddrSpec "CHANGE.ME" (DomainDotAtom $ "YOUR" :| ["DOMAIN"]))
              (x:xs) -> x :| xs
            settings = defaultReplySettings idents & set replyMode mode
          mbody <- use (asMailView . mvBody)
          let
            quoted = toQuotedMail charsets settings mbody m
            setText l t = modifying (asCompose . l . editEditorL . E.editContentsL)
                                    (insertMany t . clearZipper)
          setText cTo (views (headerTo charsets) AddressText.renderAddresses quoted)
          setText cFrom (views (headerFrom charsets) AddressText.renderAddresses quoted)
          setText cSubject (views (headerSubject charsets) fold quoted)
          setText cCc (views (headerCC charsets) AddressText.renderAddresses quoted)
          modifying (asCompose . cAttachments) (insertOrReplaceAttachment quoted)

-- | Toggles whether we want to show all headers from an e-mail or a
-- filtered list in the 'AppState'.
--
toggleHeaders :: Action 'ViewMail 'ScrollingMailView ()
toggleHeaders = Action
  { _aDescription = ["toggle mail headers"]
  , _aAction = modifying (asMailView . mvHeadersState) f
  }
  where
    f Filtered = ShowAll
    f ShowAll = Filtered

-- | Apply given tag operations on the currently selected thread or
-- mail.
--
setTags :: forall v ctx. HasToggleableList ctx => [TagOp] -> Action v ctx ()
setTags ops =
    Action
    { _aDescription = ["apply tag operations: " <> T.intercalate ", " (T.pack . show <$> ops) ]
    , _aAction = do
        w <- gets focusedViewWidget
        case w of
          ScrollingMailView ->
            toggledOrSelectedItemHelper
              (Proxy @'ScrollingMailView)
              (manageMailTags ops)
              (over _2 (Notmuch.tagItem ops))
          ListOfThreads ->
            toggledOrSelectedItemHelper
              (Proxy @'ListOfThreads)
              (manageThreadTags ops)
              (over _2 (Notmuch.tagItem ops))
          _ -> error "setTags called on widget without a registered handler"

    }

-- | Reloads the list of threads by executing the notmuch query given
-- by the search widget.
--
reloadList :: Action 'Threads 'ListOfThreads ()
reloadList = Action ["reload list of threads"] applySearch

-- | Selects the next unread mail in a thread.
--
selectNextUnread :: Action 'ViewMail 'ListOfMails ()
selectNextUnread = Action
  { _aDescription = ["select next unread"]
  , _aAction = do
      -- find by unread tag...
      p <- uses (asConfig . confNotmuch . nmNewTag) Notmuch.hasTag
      -- but if there is no resulting selection, move to the
      -- last element in the list
      let f l = maybe (L.listMoveTo (-1) l) (const l) (view L.listSelectedL l)
      modifying (asThreadsView . miListOfMails) (f . L.listFindBy (p . view _2))
  }

-- | Selects a list item. Currently only used in the file browser to
-- select a file for attaching.
--
toggleListItem :: forall v ctx. HasToggleableList ctx => Action v ctx ()
toggleListItem =
    Action
    { _aDescription = ["toggle selected state of a list item"]
    , _aAction = use (list (Proxy @ctx) . L.listSelectedL) >>= traverse_ (toggle (Proxy @ctx))
    }

untoggleListItems :: forall v ctx. HasToggleableList ctx => Action v ctx ()
untoggleListItems =
    Action
    { _aDescription = ["untoggle all selected list items"]
    , _aAction = untoggleAll (Proxy @ctx)
    }

-- | Delete an attachment from a mail currently being composed.
--
-- TODO: could this be generalised over a type class?
-- (See https://github.com/purebred-mua/purebred/issues/366)
--
delete :: Action 'ComposeView 'ComposeListOfAttachments ()
delete =
    Action
    { _aDescription = ["delete entry"]
    , _aAction = do
        len <- uses (asCompose . cAttachments . L.listElementsL) length
        if len < 2
          then
            showWarning "You may not remove the only attachment"
          else
            use (asCompose . cAttachments . L.listSelectedL)
            >>= modifying (asCompose . cAttachments) . maybe id L.listRemove
    }

-- | Adds all selected files as attachments to the e-mail.
--
createAttachments :: Action 'FileBrowser 'ListOfFiles ()
createAttachments = Action ["adds selected files as attachments"] $ do
  sel <- uses (asFileBrowser . fbEntries) FB.fileBrowserCursor
  if isFileUnderCursor sel then
    put =<< liftIO . makeAttachmentsFromSelected =<< get
    else view aAction fileBrowserToggleFile

-- | Action to deal with a choice from the confirmation dialog.
--
handleConfirm :: Action 'ComposeView 'ConfirmDialog ()
handleConfirm = Action ["handle confirmation"] keepOrDiscardDraft

-- | Edit an e-mail as a new mail. This is typically used by saving a
-- mail under composition for later and continuing the draft. Another
-- use case can be editing an already sent mail in order to send it to
-- anther recipient.
--
composeAsNew :: Action 'ViewMail 'ScrollingMailView ()
composeAsNew = Action ["edit mail as new"] $
  preuse (asThreadsView .  miListOfMails . to L.listSelectedElement . _Just . _2 . _2)
    >>= maybe
      (showWarning "No mail selected")
      (\mail -> do
        pmail <- use (asMailView . mvMail)
        charsets <- use (asConfig . confCharsets)
        runExceptT (specialCaseForDraft mail)
          >>= either showError (const $ assign asCompose (newComposeFromMail charsets pmail))
      )

-- | This is a special case workaround for draft mails used by
-- 'composeAsNew'. If the mail we're handling is a draft mail, remove
-- the draft on the file system, otherwise do nothing. That will
-- result in creating a copy mail object for non-draft mails.
--
specialCaseForDraft ::
     (MonadState AppState m, MonadIO m, MonadError Error m)
  => NotmuchMail
  -> m ()
specialCaseForDraft mail = do
  dbpath <- use (asConfig . confNotmuch . nmDatabase)
  draftTag <- use (asConfig . confNotmuch . nmDraftTag)
  when (draftTag `elem` view mailTags mail)
    $ Notmuch.mailFilepath mail dbpath >>= Notmuch.unindexFilePath dbpath


fileBrowserToggleFile :: Action 'FileBrowser 'ListOfFiles ()
fileBrowserToggleFile =
  Action ["toggle file browser file"] $ do
    fb <- use (asFileBrowser . fbEntries)
    fb' <- lift $ FB.maybeSelectCurrentEntry fb
    assign (asFileBrowser . fbEntries) fb'

-- | Search related mails sent from the same authors.
--
searchRelated :: Action 'Threads 'ListOfThreads ()
searchRelated = Action ["search related mail"] $ do
  authors <- preuse (asThreadsView . miThreads . listList . to L.listSelectedElement . _Just . _2 . _2 . thAuthors . traverse)
  case firstOf traverse authors of
    Nothing -> runExceptT (throwError (InvalidQueryError "No authors availabe to perform search"))
      >>= either showError (const $ pure ())
    Just searchterm -> do
      modifying (asThreadsView . miSearchThreadsEditor . editEditorL . E.editContentsL) (insertMany searchterm . clearZipper)
      runSearch searchterm


-- Function definitions for actions
--

-- | Traverse and make attachments from the selected files in the file
-- browser.
makeAttachmentsFromSelected :: AppState -> IO AppState
makeAttachmentsFromSelected s = do
  let toggled = view FB.fileInfoFilePathL
                <$> view (asFileBrowser . fbEntries . to FB.fileBrowserSelection) s
      selected = view FB.fileInfoFilePathL
                 <$> toListOf (asFileBrowser . fbEntries . to FB.fileBrowserCursor . traversed) s
  parts <- traverse (\x -> createAttachmentFromFile (mimeType x) x) (toggled `union` selected)
  pure $ s & over (asCompose . cAttachments) (listAppendAttachments parts)
    . over (asViews . vsFocusedView) (Brick.focusSetCurrent ComposeView)
    . set (asViews . vsViews . ix ComposeView . vFocus) ComposeListOfAttachments
  where
    listAppendAttachments parts = L.listMoveTo (-1) . over L.listElementsL (<> Vector.fromList parts)


-- | Determine if the selected directory entry is a file or not. We do
-- not support adding entire directories to the e-mail.
--
isFileUnderCursor :: Maybe FB.FileInfo -> Bool
isFileUnderCursor = maybe False (FB.fileTypeMatch [FB.RegularFile])

-- | Take the notmuch query given by the user and update the
-- 'AppState' with notmuch query result.
--
applySearch :: (MonadIO m, MonadState AppState m) => m ()
applySearch = do
  searchterms <- currentLine <$> use (asThreadsView . miSearchThreadsEditor . editEditorL . E.editContentsL)
  runSearch searchterms

runSearch :: (MonadIO m, MonadState AppState m) => T.Text -> m ()
runSearch searchterms = do
  nmconf <- use (asConfig . confNotmuch)
  r <- runExceptT (Notmuch.getThreads searchterms nmconf)
  case r of
    Left e -> showError e
    Right threads -> do
      liftIO getCurrentTime >>= assign asLocalTime
      notifyNumThreads threads
      modifying (asThreadsView . miListOfThreads) (L.listReplace threads (Just 0))
      assign (asThreadsView . miThreads . listLength) Nothing

-- | Fork a thread to compute the length of the container and send a
-- NotifyNumThreads event.  'seq' ensures that the work is actually
-- done by the spawned thread.  Increments the generation and updates
-- the 'AppState' with it.
notifyNumThreads :: (MonadState AppState m, MonadIO m, Foldable t) => t a -> m ()
notifyNumThreads l = do
  nextGen <- uses (asThreadsView . miListOfThreadsGeneration) nextGeneration
  chan <- use (asConfig . confBChan)
  void . liftIO . forkIO $
    let len = length l
    in len `seq` writeBChan chan (NotifyNumThreads len nextGen)
  assign (asThreadsView . miListOfThreadsGeneration) nextGen

-- | Operate over either toggled or a single selected list item
--
toggledOrSelectedItemHelper ::
     (HasToggleableList n, L.Splittable (T n), MonadState AppState m)
  => Proxy n
  -> ([E n] -> m b)
  -> (E n -> E n)
  -> m ()
toggledOrSelectedItemHelper proxy fx updateFx = do
  toggled <- gets (toListOf (toggledItemsL proxy))
  selected <-
    gets (toListOf (list proxy . to L.listSelectedElement . _Just . to snd))
  if null toggled
    then fx selected >> modifying (list proxy) (L.listModify updateFx)
    else fx toggled >> modifying (toggledItemsL proxy) updateFx
  pure ()

-- | Helper function to either show an error if no list item is
-- selected, or apply given action to the item (the result of
-- which is discarded)
--
selectedItemHelper
    :: (Foldable t, L.Splittable t, MonadState AppState m)
    => Getting (L.GenericList n t a) AppState (L.GenericList n t a)
    -> (a -> m b)
    -> m ()
selectedItemHelper l f = do
  item <- use l
  case L.listSelectedElement item of
    Just (_, a) -> void $ f a
    Nothing -> showWarning "No item selected"

-- | Retrieve the given tag operations from the given editor widget
-- and parse them.
--
getEditorTagOps :: HasEditor n => Proxy n -> AppState -> Either UserMessage [TagOp]
getEditorTagOps p s =
  let contents = (foldr (<>) "" $ E.getEditContents $ view (editorL p) s)
  in parseTagOps contents

-- | Apply given tag operations on all mails
--
applyTagOps
  :: (Traversable t, MonadIO m)
  => [TagOp]
  -> t (Toggleable NotmuchMail)
  -> AppState
  -> m (Either Error (t NotmuchMail))
applyTagOps ops mails s =
  let dbpath = view (asConfig . confNotmuch . nmDatabase) s
      ms = snd <$> mails
  in runExceptT (Notmuch.messageTagModify dbpath ops ms)

updateStateWithParsedMail :: (MonadIO m, MonadMask m, MonadState AppState m) => m ()
updateStateWithParsedMail = do
  db <- use (asConfig . confNotmuch . nmDatabase)
  charsets <- use (asConfig . confCharsets)
  textwidth <- use (asConfig . confMailView . mvTextWidth)
  preferredContentType <- use (asConfig . confMailView . mvPreferredContentType)
  s <- get
  selectedItemHelper (asThreadsView . miListOfMails) $ \(_, m) ->
    runExceptT (parseMail m db >>= bodyToDisplay s textwidth charsets preferredContentType)
    >>= either
      (\e -> do
        showError e
        modifying (asViews . vsFocusedView) (Brick.focusSetCurrent Threads) )
      (\(pmail, mbody) -> do
         assign (asMailView . mvMail) (Just pmail)
         assign (asMailView . mvBody) mbody
         modifying (asViews . vsFocusedView) (Brick.focusSetCurrent ViewMail)
         assign (asMailView . mvAttachments) (setEntities pmail) )
  where
    setEntities m =
      L.list MailListOfAttachments (view vector $ toListOf entities m) 0

-- | Tag the currently selected mail as /read/. This is reflected as a
-- visual change in the UI.
--
updateReadState :: (MonadState AppState m, MonadIO m) => (Tag -> TagOp) -> m ()
updateReadState con = do
  -- Also update the thread to reflect the change. We used
  -- to pull the thread out of the database again when we
  -- navigated back to the index of threads, but does not
  -- always garantee an updated tag list. See #249
  op <- con <$> use (asConfig . confNotmuch . nmNewTag)
  toggledOrSelectedItemHelper
    (Proxy @'ScrollingMailView)
    (manageMailTags [op])
    (over _2 (Notmuch.tagItem [op]))
  modifying (asThreadsView . miListOfThreads) (L.listModify (over _2 $ Notmuch.tagItem [op]))

manageMailTags ::
     (Traversable t, MonadIO m, MonadState AppState m)
  => [TagOp]
  -> t (Toggleable NotmuchMail)
  -> m ()
manageMailTags ops ms = do
  result <- applyTagOps ops ms =<< get
  case result of
    Left e -> showError e
    Right _ -> pure ()

-- | Build the MIMEMessage, sanitize filepaths, serialize and send it
--
sendMail :: (MonadState AppState m, MonadCatch m, MonadIO m) => m Bool
sendMail = do
  maildir <- use (asConfig . confNotmuch . nmDatabase)
  sentTag <- use (asConfig . confNotmuch . nmSentTag)
  buildMail $ \bs -> do
    r <- runExceptT ( do
        trySendAndCatch bs
        fp <- createSentFilePath maildir
        tryIO $ LB.writeFile fp (B.toLazyByteString bs)
        Notmuch.indexFilePath maildir fp [sentTag] )
    isRight r <$ either showError pure r

-- | Build a mail from the current @AppState@ and execute the continuation.
buildMail :: (MonadState AppState m, MonadIO m) => (B.Builder -> m a) -> m a
buildMail k = do
  attachments' <- uses (asCompose . cAttachments . L.listElementsL) toList
  mail <- case attachments' of
    [x] -> pure x
    x:xs -> do
      boundary <- getStdRandom uniform
      pure $ createMultipartMixedMessage boundary (x:|xs)
    [] ->
      -- Shouldn't happen (user should be prevented from deleting the
      -- last attachment).  If it does happen, send an empty body.
      pure $ createTextPlainMessage mempty

  case mail of
    m -> do
      charsets <- use (asConfig . confCharsets)
      now <- liftIO getZonedTime
      to' <- uses (asCompose . cTo . editEditorL)
        (either (pure []) id . AT.parseOnly AddressText.addressList . T.unlines . E.getEditContents)
      from <- uses (asCompose . cFrom . editEditorL)
        (either (pure []) id . AT.parseOnly AddressText.mailboxList . T.unlines . E.getEditContents)
      subject <- uses (asCompose . cSubject . editEditorL) (T.unlines . E.getEditContents)
      let
        m' = m
          & set (headerSubject charsets) (Just subject)
          & set (headerFrom charsets) (Single <$> from)
          & set (headerTo charsets) to'
          & set headerDate (Just now)
          & sanitizeMail charsets

      -- run pre-send hooks
      hooks <- uses (asConfig . confPlugins) (fmap (getPreSendHook . view preSendHook))
      m'' <- use asConfig >>= runReaderT (foldr (>=>) pure hooks m')

      k (buildMessage m'')

-- | Send the mail, but catch and show an error if it happened.
trySendAndCatch
  :: (MonadState AppState m, MonadIO m, MonadCatch m, MonadError Error m)
  => B.Builder -> m ()
trySendAndCatch m = do
  cmd <- use (asConfig . confComposeView . cvSendMailCmd)
  defMailboxes <- use (asConfig . confComposeView . cvIdentities)
  (liftIO (cmd m) >>= either throwError (const $ assign asCompose (initialCompose defMailboxes)))
    `catch` (throwError . SendMailError . (show :: IOException -> String))

-- | santize the mail before we send it out
-- Note: currently only strips away path names from files
sanitizeMail :: CharsetLookup -> MIMEMessage -> MIMEMessage
sanitizeMail charsets =
  over (attachments . headers . contentDisposition . traversed . filename charsets) takeFileName

initialCompose :: [Mailbox] -> Compose
initialCompose mailboxes =
  Compose
    (StatefulEditor mempty $ E.editorText ComposeFrom (Just 1) (AddressText.renderMailboxes mailboxes))
    (StatefulEditor mempty $ E.editorText ComposeTo (Just 1) "")
    (StatefulEditor mempty $ E.editorText ComposeCc (Just 1) "")
    (StatefulEditor mempty $ E.editorText ComposeBcc (Just 1) "")
    (StatefulEditor mempty $ E.editorText ComposeSubject (Just 1) "")
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
      cc = preview (_Just . headers . at "cc" . _Just . to decodeLenient) m
      bcc = preview (_Just . headers . at "bcc" . _Just . to decodeLenient) m
      attachments' =
        view vector $ toMIMEMessage charsets <$> toListOf (_Just . entities) m
      orEmpty = view (non "")
   in Compose
        (StatefulEditor mempty $ E.editorText ComposeFrom (Just 1) (orEmpty from))
        (StatefulEditor mempty $ E.editorText ComposeTo (Just 1) (orEmpty to'))
        (StatefulEditor mempty $ E.editorText ComposeCc (Just 1) (orEmpty cc))
        (StatefulEditor mempty $ E.editorText ComposeBcc (Just 1) (orEmpty bcc))
        (StatefulEditor mempty $ E.editorText ComposeSubject (Just 1) (orEmpty subject))
        (L.list ComposeListOfAttachments attachments' 1)
        initialDraftConfirmDialog

initialDraftConfirmDialog :: Dialog ConfirmDraft
initialDraftConfirmDialog = dialog (Just "Keep draft?") (Just (0, [("Keep", Keep), ("Discard", Discard)])) 50

-- | Serialise the WireEntity and write it to a temporary file. If no WireEntity
-- exists (e.g. composing a new mail) just use the empty file. When the
-- serialising fails, we return an error. Once the editor exits, read the
-- contents from the temporary file, delete it and create a MIME message out of
-- it. Set it in the Appstate.
invokeEditor' ::
     (MonadError Error m, MonadIO m, MonadState AppState m, MonadMask m)
  => m T.Text
invokeEditor' = do
  maildir <- use (asConfig . confNotmuch . nmDatabase)
  cmd <- use (asConfig . confEditor)
  maybeEntity <- preuse (asCompose . cAttachments . to L.listSelectedElement . _Just . _2 . to getTextPlainPart . _Just)
  let
    mkEntity :: (MonadError Error m) => m B.ByteString
    mkEntity = maybe (pure mempty) entityToBytes maybeEntity
    entityCmd = EntityCommand handleExitCodeTempfileContents
      (draftFileResoure maildir) (\_ fp -> proc cmd [fp]) tryReadProcessStderr
  mkEntity >>= runEntityCommand . entityCmd

-- | Write the serialised WireEntity to a temporary file. Pass the FilePath of
-- the temporary file to the command. Do not remove the temporary file, so
-- programs using sub-shells will be able to read the temporary file. Return an
-- error if either the WireEntity doesn't exist (e.g. not selected) or it can
-- not be serialised.
openCommand' :: (MonadIO m, MonadMask m, MonadState AppState m) => MailcapHandler -> m ()
openCommand' cmd = do
  let
    mkConfig :: (MonadError Error m, MonadIO m) => WireEntity -> m (EntityCommand m FilePath)
    mkConfig =
      let con = EntityCommand
            handleExitCodeThrow
            (tmpfileResource (view mhKeepTemp cmd))
            (\_ fp -> toProcessConfigWithTempfile (view mhMakeProcess cmd) fp)
            tryReadProcessStderr
      in fmap con . entityToBytes
  selectedItemHelper (asMailView . mvAttachments) $ \ent ->
    runExceptT (mkConfig ent >>= runEntityCommand)
      >>= either showError (const $ pure ())

-- | Pass the serialized WireEntity to a Bytestring as STDIN to the process. No
-- temporary file is used. If either no WireEntity exists (e.g. none selected)
-- or it can not be serialised an error is returned.
pipeCommand' :: (MonadIO m, MonadMask m, MonadState AppState m) => FilePath -> m ()
pipeCommand' cmd
  | null cmd = showWarning "Empty command"
  | otherwise = do
      let
        mkConfig :: (MonadError Error m, MonadIO m) => WireEntity -> m (EntityCommand m ())
        mkConfig =
          let con = EntityCommand
                handleExitCodeThrow
                emptyResource
                (\b _ -> setStdin (byteStringInput $ LB.fromStrict b) (proc cmd []))
                tryReadProcessStderr
          in fmap con . entityToBytes
      selectedItemHelper (asMailView . mvAttachments) $ \ent ->
        runExceptT (mkConfig ent >>= runEntityCommand)
          >>= either showError (const $ pure ())

editAttachment ::
     (MonadState AppState m, MonadIO m, MonadMask m) => ViewName -> Name -> m ()
editAttachment n w =
  selectedItemHelper (asCompose . cAttachments) $ \m ->
    case preview (headers . contentDisposition . folded . dispositionType) m of
      Just Inline ->
        let updatePart t =
              modifying
                (asCompose . cAttachments)
                (insertOrReplaceAttachment $ createTextPlainMessage t)
            errormsg e =
              ProcessError $
              "Editor exited abnormally ( " <>
              show e <> " ). Press Esc to continue."
         in runExceptT invokeEditor' >>=
            either (\e -> showError (errormsg e) *> switchMode' n w) updatePart
      _ -> showWarning "Not yet implemented. See #182"

-- | If the list is empty, insert the attachment otherwise replace the
-- currently selected item.
--
insertOrReplaceAttachment ::
  MIMEMessage
  -> L.List Name MIMEMessage
  -> L.List Name MIMEMessage
insertOrReplaceAttachment newPart l =
  case L.listSelectedElement l of
    Nothing -> L.listInsert 0 newPart l
    Just _ ->
        -- replace
        L.listModify (const newPart) l

getTextPlainPart :: MIMEMessage -> Maybe WireEntity
getTextPlainPart = firstOf (entities . filtered f)
  where
  f = matchContentType "text" (Just "plain") . view (headers . contentType)

mimeType :: FilePath -> ContentType
mimeType =
  fromRight contentTypeApplicationOctetStream
  . parseOnly parseContentType . defaultMimeLookup . T.pack

manageThreadTags ::
     (Traversable t, MonadIO m, MonadState AppState m)
  => [TagOp]
  -> t (Toggleable NotmuchThread)
  -> m ()
manageThreadTags ops ts = do
  dbpath <- use (asConfig . confNotmuch . nmDatabase)
  runExceptT
    (Notmuch.getThreadMessages dbpath $ toListOf (traversed . _2) ts)
    >>= (\ms ->
           (get >>= applyTagOps ops ms)
          >>= either showError (const $ pure ()))
    . fromRight mempty


keepOrDiscardDraft :: (MonadMask m, MonadIO m, MonadState AppState m) => m ()
keepOrDiscardDraft = do
  r <- use (asCompose . cKeepDraft . to dialogSelection)
  case r of
    Just Keep -> keepDraft
    _ -> showInfo "Draft discarded"
  modify clearMailComposition

keepDraft :: (MonadMask m, MonadState AppState m, MonadIO m) => m ()
keepDraft = buildMail $ \bs -> do
  maildir <- use (asConfig . confNotmuch . nmDatabase)
  draftTag <- use (asConfig . confNotmuch . nmDraftTag)
  runExceptT ( do
      fp <- createDraftFilePath maildir
      tryIO $ LB.writeFile fp (B.toLazyByteString bs)
      Notmuch.indexFilePath maildir fp [draftTag] )
    >>= either showError (
    const $ showInfo "Draft saved")

resetMatchingWords :: AppState -> AppState
resetMatchingWords =
  over (asMailView . mvBody) removeMatchingWords
  . over (asMailView . mvFindWordEditor . E.editContentsL) clearZipper
  . set (asMailView . mvScrollSteps) (Brick.focusRing [])

fileBrowserSetWorkingDirectory ::
     (MonadState AppState m, MonadIO m) => m ()
fileBrowserSetWorkingDirectory = do
  modifying (asFileBrowser . fbSearchPath . editEditorL) (E.applyEdit gotoEOL)
  path <- uses (asFileBrowser . fbSearchPath . editEditorL . E.editContentsL) currentLine
  pathExists <- liftIO $ doesPathExist path
  if pathExists
    then do
      fb <- use (asFileBrowser . fbEntries) >>= liftIO . FB.setWorkingDirectory path
      modifying (asFileBrowser . fbSearchPath . editEditorL . E.editContentsL) (insertMany path . clearZipper)
      modifying (asFileBrowser . fbSearchPath) saveEditorState
      assign (asFileBrowser . fbEntries) fb
    else showWarning (T.pack $ path <> " does not exist")

switchMode' :: (MonadIO m, MonadState AppState m) => ViewName -> Name -> m ()
switchMode' vn w = do
  sink <- use (asConfig . confLogSink)
  liftIO . sink . LT.pack $
    "focus on " <> show vn <> "/" <> show w
  modifying (asViews . vsFocusedView) (Brick.focusSetCurrent vn)
  assign (asViews . vsViews . at vn . _Just . vFocus) w

