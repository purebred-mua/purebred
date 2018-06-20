{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , reloadList
  , selectNextUnread
  , focusNextWidget
  ) where

import qualified Brick
import qualified Brick.Focus as Brick
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Network.Mail.Mime (Part(..), Address(..), emptyMail, Encoding(..))
import Network.Mail.Mime.Lens (lMailParts, lMailFrom, lMailTo, lMailHeaders)
import Data.Proxy
import Data.Semigroup ((<>))
import Data.Text (unlines, unpack, pack, Text)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Data.Vector.Lens (vector)
import Data.Maybe (fromMaybe)
import Data.List (union)
import System.Exit (ExitCode(..))
import System.IO (openTempFile, hClose)
import System.Directory (getTemporaryDirectory)
import System.Process (system)
import qualified Data.Vector as Vector
import Prelude hiding (readFile, unlines)
import Control.Applicative ((<|>))
import Control.Lens
       (_Just, at, toListOf, traversed, has, itoList, set, over, view,
        (&), Getting, Lens')
import Control.Monad ((>=>))
import Control.Monad.Except (runExceptT)
import Control.Exception (onException)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Catch (bracket)
import Data.Text.Zipper (currentLine, gotoEOL, clearZipper)
import qualified Storage.Notmuch as Notmuch
import Storage.ParsedMail (parseMail, getTo, getFrom, getSubject)
import Types
import Error
import UI.Utils (safeUpdate, focusedViewWidget, focusedViewName)
import UI.Views (listOfMailsView, mailView)
import Purebred.Tags (parseTagOps)

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

-- | Generalisation of reset actions, whether they reset editors back to their
-- initial state or throw away composed, but not yet sent mails.
--
class Resetable (m :: Name) where
  reset :: Proxy m -> AppState -> T.EventM Name AppState

instance Resetable 'ManageMailTagsEditor where
  reset _ s = pure $ s & over (asMailIndex . miMailTagsEditor . E.editContentsL) clearZipper

instance Resetable 'ManageThreadTagsEditor where
  reset _ s = pure $ s & over (asMailIndex . miThreadTagsEditor . E.editContentsL) clearZipper

instance Resetable 'ComposeFrom where
  reset _ s = pure $ s & over (asCompose . cFrom . E.editContentsL) clearZipper
              . resetThreadViewEditor

instance Resetable 'ComposeSubject where
  reset _ s = pure $ s & over (asCompose . cSubject . E.editContentsL) clearZipper
              . resetThreadViewEditor

instance Resetable 'ComposeTo where
  reset _ s = pure $ s & over (asCompose . cTo . E.editContentsL) clearZipper
              . resetThreadViewEditor

instance Resetable 'ListOfAttachments where
  reset _ = pure . resetThreadViewEditor

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
  switchFocus _ _ s = if has (asCompose . cMail . lMailParts . traversed) s
                    then pure $ over (asViews . vsFocusedView) (Brick.focusSetCurrent ComposeView) s
                    else pure $ s & over (asViews . vsViews . at (focusedViewName s) . _Just . vWidgets) (replaceEditor ComposeFrom)

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
  switchFocus _ _ s = if has (asCompose . cMail . lMailParts . traversed) s
                    then pure $ over (asViews . vsFocusedView) (Brick.focusSetCurrent ComposeView) s
                    else pure $ s & over (asViews . vsViews . at (focusedViewName s) . _Just . vWidgets) (replaceEditor ComposeFrom)

instance Focusable 'ViewMail 'ListOfMails where
  switchFocus _ _ = pure . over (asViews . vsViews . at ViewMail . _Just . vFocus) (Brick.focusSetCurrent ListOfMails)

instance Focusable 'Help 'ScrollingHelpView where
  switchFocus _ _ = pure . over (asViews . vsFocusedView) (Brick.focusSetCurrent Help)

instance Focusable 'ComposeView 'ListOfAttachments where
  switchFocus _ _ s = pure $ s & over (asViews . vsViews . at ComposeView . _Just . vFocus) (Brick.focusSetCurrent ListOfAttachments)
                    . over (asViews . vsViews . at Threads . _Just . vWidgets) (replaceEditor SearchThreadsEditor)

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

instance ViewTransition 'Mails 'ViewMail where
  transitionHook _ _ = set (asViews . vsViews . at ViewMail . _Just) mailView

instance ViewTransition 'ViewMail 'Mails where
  transitionHook _ _ = set (asViews . vsViews . at Mails . _Just) listOfMailsView


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


quit :: Action v ctx (T.Next AppState)
quit = Action ["quit the application"] Brick.halt

continue :: Action v ctx (T.Next AppState)
continue = Action mempty Brick.continue

invokeEditor :: Action v ctx (T.Next AppState)
invokeEditor = Action ["invoke external editor"] (Brick.suspendAndResume . liftIO . invokeEditor')

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
  ["switch mode to " <> pack (show (name (Proxy :: Proxy a)))]
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
        _ -> pure $ over (asMailIndex. miListOfMails) L.listMoveDown s
    }

listJumpToEnd :: Action v m AppState
listJumpToEnd = Action
  { _aDescription = ["move selection to last element"]
    , _aAction = \s -> case focusedViewWidget s ListOfThreads of
        ListOfThreads -> pure $ listSetSelectionEnd (asMailIndex . miListOfThreads) s
        ScrollingMailView -> pure $ listSetSelectionEnd (asMailIndex . miListOfMails) s
        ListOfAttachments -> pure $ listSetSelectionEnd (asCompose . cAttachments) s
        _ -> pure $ listSetSelectionEnd (asMailIndex. miListOfMails) s
  }

listJumpToStart :: Action v m AppState
listJumpToStart = Action
  { _aDescription = ["move selection to first element"]
    , _aAction = \s -> case focusedViewWidget s ListOfThreads of
        ListOfThreads -> pure $ over (asMailIndex . miListOfThreads) (L.listMoveTo 0) s
        ScrollingMailView -> pure $ over (asMailIndex . miListOfMails) (L.listMoveTo 0) s
        ListOfAttachments -> pure $ over (asCompose . cAttachments) (L.listMoveTo 0) s
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

-- Function definitions for actions
--
findIndexWithOffset :: Int -> (a -> Bool) -> Vector.Vector a -> Maybe Int
findIndexWithOffset i fx = fmap (i+) . Vector.findIndex fx . Vector.drop i

listSetSelectionEnd :: Lens' AppState (L.List Name a) -> AppState -> AppState
listSetSelectionEnd list s =
  let index = Vector.length $ view (list . L.listElementsL) s
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

getEditorTagOps :: Lens' AppState (E.Editor Text Name) -> AppState -> Either Error [TagOp]
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
  either setError updateMails <$> applyTagOps tagop (Vector.singleton m) s

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
    let to =
            Address
                Nothing
                (unlines $ E.getEditContents $ view (asCompose . cTo) s)
    let from =
            Address
                Nothing
                (unlines $ E.getEditContents $ view (asCompose . cFrom) s)
    let subject =
            [ ( BC.pack "Subject"
              , unlines $ E.getEditContents $ view (asCompose . cSubject) s)]
    let s' =
            s & set (asCompose . cMail . lMailTo) [to]
            . set (asCompose . cMail . lMailFrom) from
            . set (asCompose . cMail . lMailHeaders) subject
            . set (asCompose . cMail . lMailParts) [toListOf (asCompose . cAttachments . L.listElementsL . traversed) s]
    liftIO $ view (asConfig . confComposeView . cvSendMailCmd) s' (view (asCompose . cMail) s')
    pure $ set asCompose initialCompose s'

initialCompose :: Compose
initialCompose =
  let mail = emptyMail (Address Nothing "user@localhost")
  in Compose
        mail
        (E.editorText ComposeFrom (Just 1) "")
        (E.editorText ComposeTo (Just 1) "")
        (E.editorText ComposeSubject (Just 1) "")
        (L.list ListOfAttachments Vector.empty 1)


invokeEditor' :: AppState -> IO AppState
invokeEditor' s = do
  let editor = view (asConfig . confEditor) s
  tmpfile <- attachmentFilename s
  status <- onException (system (editor <> " " <> tmpfile)) (pure $ setError editorError)
  case status of
    ExitFailure _ -> pure $ s & over (asViews . vsFocusedView) (Brick.focusSetCurrent Mails)
                              & setError editorError
    ExitSuccess -> do
      body <- liftIO $ makePlainPart tmpfile
      pure $ s & over (asCompose . cAttachments) (upsertPart body)

upsertPart :: Part -> L.List Name Part -> L.List Name Part
upsertPart newPart list =
  case L.listSelectedElement list of
    Nothing -> L.listInsert 0 newPart list
    Just (_, part) ->
      if partFilename part == partFilename newPart then
        L.listModify (const newPart) list
      else
        L.listInsert 0 newPart list

attachmentFilename :: AppState -> IO String
attachmentFilename s = let tempfile = getTemporaryDirectory >>= \tdir -> emptyTempFile tdir "purebred.tmp"
                       in case L.listSelectedElement $ view (asCompose . cAttachments) s of
                            Nothing -> tempfile
                            Just (_, p) -> maybe tempfile (pure . unpack) $ partFilename p

makePlainPart :: String -> IO Part
makePlainPart filename = do
  content <- L.readFile filename
  pure $ Part "text/plain; charset=utf-8" Base64 (Just $ pack filename) [] content

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
