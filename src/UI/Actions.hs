{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module UI.Actions (
  Scrollable(..)
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
  , focusNextWidget
  , toggleListItem
  , enterDirectory
  , parentDirectory
  , createAttachments
  , delete
  ) where

import qualified Brick
import qualified Brick.Focus as Brick
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Network.Mime (defaultMimeLookup)
import Data.Proxy
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.Vector.Lens (vector)
import Data.Maybe (fromMaybe)
import Data.List (union)
import System.Exit (ExitCode(..))
import System.IO (openTempFile, hClose)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Process (system)
import System.FilePath (takeFileName, takeDirectory, (</>))
import qualified Data.Vector as Vector
import Prelude hiding (readFile, unlines)
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Control.Lens
       (_Just, to, at, ix, _1, _2, toListOf, traversed, has, snoc,
        filtered, itoList, set, over, preview, view, (&), nullOf, firstOf,
        traversed, traverse, Getting, Lens')
import Control.Monad ((>=>))
import Control.Monad.Except (runExceptT)
import Control.Exception (onException, catch, IOException)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Catch (bracket)
import Data.Text.Zipper
       (insertMany, currentLine, gotoEOL, clearZipper)
import Data.Time.Clock (getCurrentTime)

import Data.RFC5322 (Message(..))
import qualified Data.RFC5322.Address.Text as AddressText (renderMailboxes)
import Data.MIME
       (createMultipartMixedMessage, contentTypeApplicationOctetStream,
        createTextPlainMessage, createAttachmentFromFile, renderMessage,
        contentDisposition, dispositionType, headers, filename,
        parseContentType, attachments, isAttachment, entities,
        matchContentType, contentType, mailboxList, renderMailboxes,
        addressList, renderAddresses, renderRFC5422Date, MIMEMessage,
        WireEntity, DispositionType(..), ContentType(..), Mailbox(..))
import qualified Storage.Notmuch as Notmuch
import Storage.ParsedMail (parseMail, getTo, getFrom, getSubject)
import Types
import UI.Utils
       (safeUpdate, focusedViewWidget, focusedViewName, selectedFiles)
import UI.Views (listOfMailsView, mailView)
import Purebred.Tags (parseTagOps)
import Purebred.System.Directory (listDirectory')

class Scrollable (n :: Name) where
  makeViewportScroller :: Proxy n -> Brick.ViewportScroll Name

instance Scrollable 'ScrollingMailView where
  makeViewportScroller _ = Brick.viewportScroll ScrollingMailView

instance Scrollable 'ScrollingHelpView where
  makeViewportScroller _ = Brick.viewportScroll ScrollingHelpView

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
  complete _ s = liftIO $ completeMailTags s >>= pure . over (asMailIndex . miMailTagsEditor . E.editContentsL) clearZipper

instance Completable 'ListOfAttachments where
  complete _ = sendMail

completeMailTags :: AppState -> IO AppState
completeMailTags s =
    case getEditorTagOps (asMailIndex . miMailTagsEditor) s of
        Left err -> pure $ setError err s
        Right ops' -> selectedItemHelper (asMailIndex . miListOfMails) s (manageMailTags s ops')

-- | Applying tag operations on threads
-- Note: notmuch does not support adding tags to threads themselves, instead we'll
-- apply all tag operations on mails in the thread. Instead of reloading the
-- thread, we'll apply all tag operations on the thread type as well, which are
-- not persisted to the database. This strategy is faster since it does not need
-- any database access above tagging mails, but it could pose a problem if tags
-- don't show up in the UI.
--
instance Completable 'ManageThreadTagsEditor where
  complete _ s = case getEditorTagOps (asMailIndex . miThreadTagsEditor) s of
                      Left err -> pure $ setError err s
                      Right ops ->
                        selectedItemHelper (asMailIndex . miListOfThreads) s (manageThreadTags s ops)
                        >>= pure
                        . over (asViews . vsViews . at (focusedViewName s) . _Just . vWidgets) (replaceEditor SearchThreadsEditor)

instance Completable 'ManageFileBrowserSearchPath where
  complete _ s =
    ($ s)
    <$> (either setError updateBrowseFileContents
         <$> runExceptT (listDirectory' (currentLine $ view (asFileBrowser . fbSearchPath . E.editContentsL) s)))

-- | Generalisation of reset actions, whether they reset editors back to their
-- initial state or throw away composed, but not yet sent mails.
--
class Resetable (m :: Name) where
  reset :: Proxy m -> AppState -> T.EventM Name AppState

instance Resetable 'SearchThreadsEditor where
  reset _ = pure

instance Resetable 'ManageMailTagsEditor where
  reset _ s = pure $ s & over (asMailIndex . miMailTagsEditor . E.editContentsL) clearZipper

instance Resetable 'ManageThreadTagsEditor where
  reset _ s = pure $ s
              & over (asMailIndex . miThreadTagsEditor . E.editContentsL) clearZipper
              . over (asViews . vsViews . at (focusedViewName s) . _Just . vWidgets) (replaceEditor SearchThreadsEditor)

instance Resetable 'ComposeFrom where
  reset _ s = let mailboxes = AddressText.renderMailboxes $ view (asConfig . confComposeView . cvIdentities) s
              in pure $ s & over (asCompose . cFrom . E.editContentsL)
                 (insertMany mailboxes . clearZipper)
                 . resetThreadViewEditor

instance Resetable 'ComposeSubject where
  reset _ s = pure $ s & over (asCompose . cSubject . E.editContentsL) clearZipper
              . resetThreadViewEditor

instance Resetable 'ComposeTo where
  reset _ s = pure $ s & over (asCompose . cTo . E.editContentsL) clearZipper
              . resetThreadViewEditor

instance Resetable 'ListOfAttachments where
  reset _ = pure . resetThreadViewEditor

instance Resetable 'ManageFileBrowserSearchPath where
  reset _ = pure . over (asFileBrowser . fbSearchPath . E.editContentsL) clearZipper

resetThreadViewEditor :: AppState -> AppState
resetThreadViewEditor s = over (asViews . vsViews . at (focusedViewName s) . _Just . vWidgets) (replaceEditor SearchThreadsEditor) s

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
                  . over (asViews . vsViews . at (focusedViewName s) . _Just . vWidgets) (replaceEditor ManageThreadTagsEditor)

instance Focusable 'Threads 'ComposeFrom where
  switchFocus = focusComposeFrom

instance Focusable 'Threads 'ComposeTo where
  switchFocus _ _ s = pure $ over (asViews . vsViews . at (focusedViewName s) . _Just . vWidgets) (replaceEditor ComposeTo) s

instance Focusable 'Threads 'ComposeSubject where
  switchFocus _ _ s = pure $ over (asViews. vsViews . at (focusedViewName s) . _Just . vWidgets) (replaceEditor ComposeSubject) s

instance Focusable 'Threads 'ListOfThreads where
  -- Reload the threads tags when returning to the list of threads, since they
  -- could have changed. If no thread is selected (e.g. invalid search leading
  -- no results) then there is nothing to update.
  switchFocus _ _ s = let selected = L.listSelectedElement $ view (asMailIndex . miListOfThreads) s
                    in ($ s) <$> maybe (pure id) (reloadThreadTags s) selected

instance Focusable 'Mails 'ManageMailTagsEditor where
  switchFocus _ _ = pure . over (asMailIndex . miMailTagsEditor . E.editContentsL) clearZipper
                    . over (asViews . vsViews . at Mails . _Just . vWidgets) (\l -> l `union` [ManageMailTagsEditor])

instance Focusable 'ViewMail 'ManageMailTagsEditor where
  switchFocus _ _ s = pure $ s & over (asViews. vsViews . at ViewMail . _Just . vWidgets) (\l -> l `union` [ManageMailTagsEditor])
                      . over (asViews . vsViews . at ViewMail . _Just . vFocus) (Brick.focusSetCurrent ManageMailTagsEditor)

instance Focusable 'ViewMail 'ScrollingMailView where
  -- TODO would drop whatever widget is at the end of the widgets
  switchFocus _ _ s = pure $ over (asViews. vsViews . at ViewMail . _Just . vWidgets) (reverse . drop 1 . reverse) s

instance Focusable 'Mails 'ListOfMails where
  switchFocus _ _ = pure

instance Focusable 'Mails 'ComposeFrom where
  switchFocus = focusComposeFrom

instance Focusable 'ViewMail 'ListOfMails where
  switchFocus _ _ = pure . over (asViews . vsViews . at ViewMail . _Just . vFocus) (Brick.focusSetCurrent ListOfMails)

instance Focusable 'Help 'ScrollingHelpView where
  switchFocus _ _ = pure . over (asViews . vsFocusedView) (Brick.focusSetCurrent Help)

instance Focusable 'ComposeView 'ListOfAttachments where
  switchFocus _ _ s = pure $ s & over (asViews . vsViews . at ComposeView . _Just . vFocus) (Brick.focusSetCurrent ListOfAttachments)
                    . over (asViews . vsViews . at Threads . _Just . vWidgets) (replaceEditor SearchThreadsEditor)

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
        else pure $ s &
             over
                 (asViews . vsViews . at (focusedViewName s) . _Just . vWidgets)
                 (replaceEditor ComposeFrom) .
             over (asCompose . cFrom) (E.applyEdit gotoEOL)

-- TODO: helper function to replace whatever editor we're displaying at the
-- bottom with a new editor given by name. There is currently nothing which
-- checks if we're actually replacing an editor.
-- uses ViewPatterns
replaceEditor :: Name -> [Name] -> [Name]
replaceEditor n xs = reverse (drop 1 (reverse xs)) ++ [n]

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

instance HasName 'ListOfAttachments where
  name _ = ListOfAttachments

instance HasName 'ListOfFiles where
  name _ = ListOfFiles

instance HasName 'ManageFileBrowserSearchPath where
  name _ = ManageFileBrowserSearchPath

-- | Allow to change the view to a different view in order to put the focus on a widget there
class ViewTransition (v :: ViewName) (v' :: ViewName) where
  transitionHook :: Proxy v -> Proxy v' -> AppState -> AppState
  transitionHook _ _ = id

instance ViewTransition v v where

instance ViewTransition 'Mails 'Threads where
  transitionHook _ _ = set (asViews . vsViews . at Mails . _Just) listOfMailsView

instance ViewTransition 'Threads 'Mails where

instance ViewTransition 'Threads 'ComposeView where

instance ViewTransition 'Threads 'ViewMail where

instance ViewTransition 'Help v where

instance ViewTransition v 'Help where

instance ViewTransition 'ComposeView 'Threads where

instance ViewTransition 'ComposeView 'FileBrowser where

instance ViewTransition 'Mails 'ViewMail where
  transitionHook _ _ = set (asViews . vsViews . at ViewMail . _Just) mailView

instance ViewTransition 'ViewMail 'Mails where
  transitionHook _ _ = set (asViews . vsViews . at Mails . _Just) listOfMailsView

instance ViewTransition 'FileBrowser 'ComposeView where


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

edit :: Action 'ComposeView 'ListOfAttachments (T.Next AppState)
edit = Action ["edit file"] (Brick.suspendAndResume . liftIO . editAttachment)

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
      . over (asViews . vsViews . at (viewname (Proxy :: Proxy v')) . _Just . vFocus) (Brick.focusSetCurrent $ name (Proxy :: Proxy ctx'))

done :: forall a v. (HasViewName v, Completable a) => Action v a AppState
done = Action ["apply"] (complete (Proxy :: Proxy a))

abort :: forall a v. (HasViewName v, Resetable a) => Action v a AppState
abort = Action ["cancel"] (reset (Proxy :: Proxy a))

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

listUp :: Action v m AppState
listUp =
    Action
    { _aDescription = ["mail index up one e-mail"]
    , _aAction = \s -> case focusedViewWidget s ListOfThreads of
        ListOfThreads -> pure $ over (asMailIndex . miListOfThreads) L.listMoveUp s
        ScrollingMailView -> pure $ over (asMailIndex . miListOfMails) L.listMoveUp s
        ListOfAttachments -> pure $ over (asCompose . cAttachments) L.listMoveUp s
        ListOfFiles -> pure $ over (asFileBrowser . fbEntries) L.listMoveUp s
        _ -> pure $ over (asMailIndex . miListOfMails) L.listMoveUp s
    }

listDown :: Action v m AppState
listDown =
    Action
    { _aDescription = ["mail index down one e-mail"]
    , _aAction = \s -> case focusedViewWidget s ListOfThreads of
        ListOfThreads -> pure $ over (asMailIndex . miListOfThreads) L.listMoveDown s
        ScrollingMailView -> pure $ over (asMailIndex . miListOfMails) L.listMoveDown s
        ListOfAttachments -> pure $ over (asCompose . cAttachments) L.listMoveDown s
        ListOfFiles -> pure $ over (asFileBrowser . fbEntries) L.listMoveDown s
        _ -> pure $ over (asMailIndex. miListOfMails) L.listMoveDown s
    }

listJumpToEnd :: Action v m AppState
listJumpToEnd = Action
  { _aDescription = ["move selection to last element"]
    , _aAction = \s -> case focusedViewWidget s ListOfThreads of
        ListOfThreads -> pure $ listSetSelectionEnd (asMailIndex . miListOfThreads) s
        ScrollingMailView -> pure $ listSetSelectionEnd (asMailIndex . miListOfMails) s
        ListOfAttachments -> pure $ listSetSelectionEnd (asCompose . cAttachments) s
        ListOfFiles -> pure $ listSetSelectionEnd (asFileBrowser . fbEntries) s
        _ -> pure $ listSetSelectionEnd (asMailIndex. miListOfMails) s
  }

listJumpToStart :: Action v m AppState
listJumpToStart = Action
  { _aDescription = ["move selection to first element"]
    , _aAction = \s -> case focusedViewWidget s ListOfThreads of
        ListOfThreads -> pure $ over (asMailIndex . miListOfThreads) (L.listMoveTo 0) s
        ScrollingMailView -> pure $ over (asMailIndex . miListOfMails) (L.listMoveTo 0) s
        ListOfAttachments -> pure $ over (asCompose . cAttachments) (L.listMoveTo 0) s
        ListOfFiles -> pure $ over (asFileBrowser . fbEntries) (L.listMoveTo 0) s
        _ -> pure $ over (asMailIndex. miListOfMails) (L.listMoveTo 0) s
  }

switchComposeEditor :: Action 'Threads 'ListOfThreads AppState
switchComposeEditor =
    Action
    { _aDescription = ["switch to compose editor"]
    , _aAction = \s -> if has (asCompose . cAttachments . traversed) s
                          then pure $ over (asViews . vsFocusedView) (Brick.focusSetCurrent ComposeView) s
                          else pure s
    }

replyMail :: Action 'Mails 'ListOfMails AppState
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
    , _aAction = \s -> case focusedViewWidget s ListOfThreads of
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
             vec = view (asMailIndex . miListOfMails . L.listElementsL) s
             cur = view (asMailIndex . miListOfMails . L.listSelectedL) s
             fx = Notmuch.hasTag (view (asConfig . confNotmuch . nmNewTag) s)
           in
             pure $
               over
                 (asMailIndex . miListOfMails)
                 (L.listMoveTo (maybe 0 (\i -> seekIndex i fx vec) cur))
                 s
         }

focusNextWidget :: Action v w AppState
focusNextWidget =
    Action
    { _aDescription = ["moves input focus to the next widget"]
    , _aAction = \s -> pure $
                      over (asViews . vsViews . at (focusedViewName s) . _Just . vFocus) Brick.focusNext s
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

delete :: Action 'ComposeView 'ListOfAttachments AppState
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
    . over (asViews . vsViews . at ComposeView . _Just . vFocus) (Brick.focusSetCurrent ListOfFiles)
  where
    go :: [MIMEMessage] -> L.List Name MIMEMessage -> L.List Name MIMEMessage
    go parts list = foldr upsertPart list parts
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

findIndexWithOffset :: Int -> (a -> Bool) -> Vector.Vector a -> Maybe Int
findIndexWithOffset i fx = fmap (i+) . Vector.findIndex fx . Vector.drop i

listSetSelectionEnd :: Lens' AppState (L.List Name a) -> AppState -> AppState
listSetSelectionEnd list s =
  let index = view (list . L.listElementsL . to length) s
  in over list (L.listMoveTo index) s

-- | Seek forward from an offset, returning the offset if
-- nothing after it matches the predicate.
--
seekIndex :: Int -> (a -> Bool) -> Vector.Vector a -> Int
seekIndex i f = fromMaybe i . findIndexWithOffset i f

applySearch :: AppState -> T.EventM Name AppState
applySearch s = runExceptT (Notmuch.getThreads searchterms (view (asConfig . confNotmuch) s))
                >>= pure . ($ s) . either setError updateList
   where searchterms = currentLine $ view (asMailIndex . miSearchThreadsEditor . E.editContentsL) s
         updateList vec s' =
           let current = view (asMailIndex . miListOfThreads . L.listSelectedL) s' <|> Just 0
           in over (asMailIndex . miListOfThreads) (L.listReplace vec current) s'

setMailsForThread :: AppState -> IO AppState
setMailsForThread s = selectedItemHelper (asMailIndex . miListOfThreads) s $ \(_, t) ->
  let dbpath = view (asConfig . confNotmuch . nmDatabase) s
      updateThreadMails vec = over (asMailIndex . miListOfMails) (L.listReplace vec (Just 0))
  in either setError updateThreadMails <$> runExceptT (Notmuch.getThreadMessages dbpath t)

selectedItemHelper
    :: Applicative f
    => Getting (L.List n t) AppState (L.List n t)
    -> AppState
    -> ((Int, t) -> f (AppState -> AppState))
    -> f AppState
selectedItemHelper l s func =
  ($ s) <$> case L.listSelectedElement (view l s) of
  Just m -> func m
  Nothing -> pure $ setError (GenericError "No item selected.")

getEditorTagOps :: Lens' AppState (E.Editor T.Text Name) -> AppState -> Either Error [TagOp]
getEditorTagOps widget s =
  let contents = (foldr (<>) "" $ E.getEditContents $ view widget s)
  in parseTagOps contents

applyTagOps
  :: (Traversable t, MonadIO m)
  => [TagOp]
  -> t (a, NotmuchMail)
  -> AppState
  -> m (Either Error (t (a, NotmuchMail)))
applyTagOps ops mails s =
  let dbpath = view (asConfig . confNotmuch . nmDatabase) s
  in runExceptT (Notmuch.messageTagModify dbpath ops mails)

updateStateWithParsedMail :: AppState -> IO AppState
updateStateWithParsedMail s = selectedItemHelper (asMailIndex . miListOfMails) s $ \(_, m) ->
        either
            (\e -> setError e . over (asViews . vsFocusedView) (Brick.focusSetCurrent Threads))
            (\pmail -> set (asMailView . mvMail) (Just pmail) . over (asViews . vsFocusedView) (Brick.focusSetCurrent ViewMail))
            <$> runExceptT (parseMail m (view (asConfig . confNotmuch . nmDatabase) s))

updateReadState :: TagOp -> AppState -> IO AppState
updateReadState op s = selectedItemHelper (asMailIndex . miListOfMails) s (manageMailTags s [op])

manageMailTags
    :: MonadIO m
    => AppState
    -> [TagOp]
    -> (Int, NotmuchMail)
    -> m (AppState -> AppState)
manageMailTags s tagop m =
  either setError updateMails <$> applyTagOps tagop [m] s

updateMails :: Foldable t => t (Int, NotmuchMail) -> AppState -> AppState
updateMails mails = over (asMailIndex . miListOfMails . L.listElementsL) (`safeUpdate` mails)

setError :: Error -> AppState -> AppState
setError = set asError . Just

replyToMail :: AppState -> T.EventM Name AppState
replyToMail s =
  pure . ($ s)
  =<< case L.listSelectedElement (view (asMailIndex . miListOfMails) s) of
    Just (_, m) -> either handleErr handleMail
                   <$> runExceptT (parseMail m (view (asConfig . confNotmuch . nmDatabase) s))
    Nothing -> pure id
  where
    handleErr e = over (asViews . vsFocusedView) (Brick.focusSetCurrent Threads) . setError e
    handleMail pmail s' = s' &
      set (asCompose . cTo) (E.editor ComposeTo Nothing $ getFrom pmail)
      . set (asCompose . cFrom) (E.editor ComposeFrom Nothing $ getTo pmail)
      . set (asCompose . cSubject)
        (E.editor ComposeSubject Nothing ("Re: " <> getSubject pmail))
      . over (asViews . vsViews . at (focusedViewName s) . _Just . vWidgets) (replaceEditor ComposeFrom)

sendMail :: AppState -> T.EventM Name AppState
sendMail s = do
    dateTimeNow <- liftIO getCurrentTime
    let to' = either (pure []) id $ parseOnly addressList $ T.encodeUtf8 $ T.unlines $ E.getEditContents $ view (asCompose . cTo) s
        from = either (pure []) id $ parseOnly mailboxList $ T.encodeUtf8 $ T.unlines $ E.getEditContents $ view (asCompose . cFrom) s
        subject = T.unlines $ E.getEditContents $ view (asCompose . cSubject) s
        attachments' = toListOf (asCompose . cAttachments . L.listElementsL . traversed) s
        (b, l') = splitAt 50 $ view (asConfig . confComposeView . cvBoundary) s
        mail = if has (asCompose . cAttachments . L.listElementsL . traversed . filtered isAttachment) s
                then Just $ createMultipartMixedMessage (C8.pack b) attachments'
                else firstOf (asCompose . cAttachments . L.listElementsL . traversed) s
    case mail of
        Nothing -> pure $ setError (GenericError "Black hole detected") s
        (Just m) -> let m' = m
                          & set (headers . at "Subject") (Just $ T.encodeUtf8 subject)
                          . set (headers . at "From") (Just $ renderMailboxes from)
                          . set (headers . at "To") (Just $ renderAddresses to')
                          . set (headers . at "Date") (Just $ renderRFC5422Date dateTimeNow)
                    in liftIO $ trySendAndCatch l' (renderMessage $ sanitizeMail m') s

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
        defMailboxes = view (asConfig . confComposeView . cvIdentities) s
    catch
        (cmd m $> (s
         & set asCompose (initialCompose defMailboxes)
         . set (asConfig . confComposeView . cvBoundary) l'))
        (\e ->
              let err = show (e :: IOException)
              in pure $ s & setError (SendMailError err))

-- | santize the mail before we send it out
-- Note: currently only strips away path names from files
sanitizeMail :: MIMEMessage -> MIMEMessage
sanitizeMail = over (attachments . headers . contentDisposition . filename) (T.pack . takeFileName . T.unpack)

initialCompose :: [Mailbox] -> Compose
initialCompose mailboxes =
  let mail = B.empty
  in Compose
        mail
        (E.editorText ComposeFrom (Just 1) (AddressText.renderMailboxes mailboxes))
        (E.editorText ComposeTo (Just 1) "")
        (E.editorText ComposeSubject (Just 1) "")
        (L.list ListOfAttachments mempty 1)


invokeEditor' :: AppState -> IO AppState
invokeEditor' s = do
  let editor = view (asConfig . confEditor) s
  let m = preview (asCompose . cAttachments . to L.listSelectedElement
                     . _Just . _2 . to getTextPlainPart . _Just) s
  tmpfile <- getTempFileForEditing m
  status <- onException (system (editor <> " " <> tmpfile)) (pure $ setError editorError)
  case status of
    ExitFailure _ -> pure $ s & over (asViews . vsFocusedView) (Brick.focusSetCurrent Mails)
                              & setError editorError
    ExitSuccess -> do
      contents <- T.readFile tmpfile
      removeIfExists tmpfile
      let mail = createTextPlainMessage contents
      pure $ s & over (asCompose . cAttachments) (upsertPart mail)

removeIfExists :: FilePath -> IO ()
removeIfExists fp = removeFile fp `catch` handleError
  where
    handleError :: IOError -> IO ()
    handleError _ = pure ()

editAttachment :: AppState -> IO AppState
editAttachment s =
    case L.listSelectedElement $ view (asCompose . cAttachments) s of
        Nothing -> pure $ setError (GenericError "No file selected to edit") s
        Just (_, m) -> case preview (headers . contentDisposition . dispositionType) m of
          (Just Inline) -> invokeEditor' s
          _ -> pure $ setError (GenericError "Not implemented. See #182") s

upsertPart :: MIMEMessage -> L.List Name MIMEMessage -> L.List Name MIMEMessage
upsertPart newPart list =
  case L.listSelectedElement list of
    Nothing -> L.listInsert 0 newPart list
    Just (_, part) ->
      if view (headers . contentDisposition . filename) part == view (headers . contentDisposition . filename) newPart then
        -- replace
        L.listModify (const newPart) list
      else
        -- append
        list & over L.listElementsL (`snoc` newPart)
             . set L.listSelectedL (Just (view (L.listElementsL . to length) list))

-- | Helper which writes the contents of the mail into the file, otherwise
-- return an empty filepath
getTempFileForEditing :: Maybe WireEntity -> IO String
getTempFileForEditing m = do
    tempfile <- getTemporaryDirectory >>= \tdir -> emptyTempFile tdir "purebred.txt"
    f tempfile m
 where
   f fp (Just (Message _ body)) = B.writeFile fp body >> pure fp
   f fp _ = pure fp

getTextPlainPart :: MIMEMessage -> Maybe WireEntity
getTextPlainPart = firstOf (entities . filtered f)
  where
  f = matchContentType "text" (Just "plain") . view (headers . contentType)

mimeType :: FilePath -> ContentType
mimeType x = let parsed = parseOnly parseContentType $ defaultMimeLookup (T.pack x)
             in either (const contentTypeApplicationOctetStream) id parsed

editorError :: Error
editorError = GenericError ("Editor command exited with error code."
  <> " Check your editor configuration and your terminal.")

emptyTempFile :: FilePath -> String -> IO FilePath
emptyTempFile targetDir template = bracket
  (openTempFile targetDir template)
  (\(_, handle) -> hClose handle)
  (\(filePath, _) -> pure filePath)

manageThreadTags
    :: MonadIO m
    => AppState
    -> [TagOp]
    -> (t, NotmuchThread)
    -> m (AppState -> AppState)
manageThreadTags s ops t =
  let update ops' _ = over (asMailIndex . miListOfThreads) (L.listModify (Notmuch.tagItem ops'))
  in getMailsForThread t s
     >>= \ms -> applyTagOps ops ms s
     >>= either (pure . setError) (pure . update ops)

getMailsForThread
    :: MonadIO f
    => (t, NotmuchThread)
    -> AppState
    -> f (Vector.Vector (Int, NotmuchMail))
getMailsForThread (_, ts) s =
  let dbpath = view (asConfig . confNotmuch . nmDatabase) s
  in either (const mempty)(view vector . itoList) <$> runExceptT (Notmuch.getThreadMessages dbpath ts)

reloadThreadTags
  :: MonadIO m
  => AppState
  -> (a, NotmuchThread)
  -> m (AppState -> AppState)
reloadThreadTags s (_, thread) =
  let dbpath = view (asConfig. confNotmuch . nmDatabase) s
      updateList t' = over (asMailIndex . miListOfThreads) (L.listModify $ const t')
  in either setError updateList <$> runExceptT (Notmuch.reloadThreadTags dbpath thread)
