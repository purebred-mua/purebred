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
  , nextInput
  , previousInput
  , listUp
  , listDown
  , switchComposeEditor
  , replyMail
  , scrollUp
  , scrollDown
  , toggleHeaders
  , initialCompose
  , continue
  , chain
  , chain'
  , setTags
  , invokeEditor
  , reloadList
  , selectNextUnread
  ) where

import qualified Brick.Main as Brick
       (suspendAndResume, continue, halt, vScrollPage, viewportScroll, ViewportScroll)
import qualified Brick.Focus as Brick
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Network.Mail.Mime (Address(..), renderSendMail, simpleMail')
import Data.Proxy
import Data.Semigroup ((<>))
import Data.Text (unlines, Text)
import Data.Text.Lazy.IO (readFile)
import Data.Vector.Lens (vector)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import System.Exit (ExitCode(..))
import System.IO (openTempFile, hClose)
import System.Directory (getTemporaryDirectory)
import System.Process (system)
import qualified Data.Vector as Vector
import Prelude hiding (readFile, unlines)
import Control.Applicative ((<|>))
import Control.Lens
       (itoList, set, over, view, _Just, (&), Getting, Lens')
import Control.Lens.Fold ((^?!))
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
import UI.Utils (safeUpdate)
import Purebred.Tags (parseTagOps)

class Scrollable (n :: Mode) where
  makeViewportScroller :: Proxy n -> Brick.ViewportScroll Name

instance Scrollable 'ViewMail where
  makeViewportScroller _ = Brick.viewportScroll ScrollingMailView

instance Scrollable 'Help where
  makeViewportScroller _ = Brick.viewportScroll ScrollingHelpView


class ModeTransition (s :: Mode) (d :: Mode) where

instance ModeTransition s s where

instance ModeTransition 'ManageMailTags 'BrowseMail where

instance ModeTransition 'BrowseThreads 'SearchThreads where

instance ModeTransition 'BrowseMail 'ManageMailTags where

instance ModeTransition 'BrowseThreads 'ManageThreadTags where

instance ModeTransition 'ViewMail 'BrowseMail where

instance ModeTransition 'ViewMail 'ManageMailTags where

instance ModeTransition 'BrowseThreads 'BrowseMail where

instance ModeTransition 'ManageThreadTags 'BrowseThreads where

instance ModeTransition 'BrowseMail 'BrowseThreads  where

instance ModeTransition 'SearchThreads 'BrowseThreads  where

instance ModeTransition 'BrowseThreads 'GatherHeadersFrom where

instance ModeTransition 'BrowseMail 'GatherHeadersFrom where

instance ModeTransition 'GatherHeadersFrom 'BrowseThreads where

instance ModeTransition 'GatherHeadersFrom 'GatherHeadersTo where

instance ModeTransition 'GatherHeadersTo 'BrowseThreads where

instance ModeTransition 'GatherHeadersTo 'GatherHeadersSubject where

instance ModeTransition 'GatherHeadersSubject 'BrowseThreads where

instance ModeTransition 'GatherHeadersSubject 'ComposeEditor where

instance ModeTransition 'ComposeEditor 'BrowseThreads where

instance ModeTransition 'Help 'BrowseThreads where

instance ModeTransition 'ComposeEditor 'AddAttachment where

instance ModeTransition 'AddAttachment 'ComposeEditor where

instance ModeTransition s 'Help where  -- help can be reached from any mode

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
class Completable (m :: Mode) where
  complete :: Proxy m -> AppState -> T.EventM Name AppState

instance Completable 'SearchThreads where
  complete _ = applySearch

instance Completable 'ComposeEditor where
  complete _ = sendMail

instance Completable 'ManageMailTags where
  complete _ = liftIO . completeMailTags

completeMailTags :: AppState -> IO AppState
completeMailTags s =
    let selected = toList $ L.listSelectedElement $ view (asMailIndex . miListOfMails) s
    in case getEditorTagOps (asMailIndex . miMailTagsEditor) s of
        Left err -> pure $ setError err s
        Right ops' -> ($ s)
          <$> (applyTagOps ops' (view vector selected) s
               >>= either (pure . setError) (pure . updateMails))

-- | Applying tag operations on threads
-- Note: notmuch does not support adding tags to threads themselves, instead we'll
-- apply all tag operations on mails in the thread. Instead of reloading the
-- thread, we'll apply all tag operations on the thread type as well, which are
-- not persisted to the database. This strategy is faster since it does not need
-- any database access above tagging mails, but it could pose a problem if tags
-- don't show up in the UI.
--
instance Completable 'ManageThreadTags where
  complete _ s = case getEditorTagOps (asMailIndex . miThreadTagsEditor) s of
                      Left err -> pure $ setError err s
                      Right ops -> selectedItemHelper (asMailIndex . miListOfThreads) s (manageThreadTags s ops)

-- | Generalisation of reset actions, whether they reset editors back to their
-- initial state or throw away composed, but not yet sent mails.
--
class Resetable (m :: Mode) where
  reset :: Proxy m -> AppState -> T.EventM Name AppState

instance Resetable 'ComposeEditor where
  reset _ = pure . set asCompose initialCompose

instance Resetable 'ManageMailTags where
  reset _ = pure . over (asMailIndex . miMailTagsEditor . E.editContentsL) clearZipper

instance Resetable 'ManageThreadTags where
  reset _ = pure . over (asMailIndex . miThreadTagsEditor . E.editContentsL) clearZipper

-- | Generalisation of focus changes between widgets on the same "view"
-- expressed with the mode in the application state.
--
class Focusable (m :: Mode) where
  switchFocus :: Proxy m -> AppState -> T.EventM Name AppState

instance Focusable 'SearchThreads where
  switchFocus _ = pure . over (asMailIndex . miSearchThreadsEditor) (E.applyEdit gotoEOL)

instance Focusable 'ManageMailTags where
  switchFocus _ = pure . over (asMailIndex . miMailTagsEditor . E.editContentsL) clearZipper

instance Focusable 'ManageThreadTags where
  switchFocus _ = pure . over (asMailIndex . miThreadTagsEditor . E.editContentsL) clearZipper

instance Focusable 'BrowseMail where
  switchFocus _ = pure

instance Focusable 'GatherHeadersFrom where
  switchFocus _ s = case view (asCompose . cTmpFile) s of
                          Just _ -> pure $ set asAppMode ComposeEditor s
                          Nothing -> pure $ set asAppMode GatherHeadersFrom s

instance Focusable 'GatherHeadersTo where
  switchFocus _ = pure . set asAppMode GatherHeadersTo

instance Focusable 'GatherHeadersSubject where
  switchFocus _ = pure . set asAppMode GatherHeadersSubject

instance Focusable 'ComposeEditor where
  switchFocus _ = pure

instance Focusable 'BrowseThreads where
  switchFocus _ = pure . set asAppMode BrowseThreads

instance Focusable 'Help where
  switchFocus _ = pure . set asAppMode Help

instance Focusable 'AddAttachment where
  switchFocus _ = pure

-- | Problem: How to chain actions, which operate not on the same mode, but a
-- mode switched by the previous action?
class HasMode (a :: Mode) where
  mode :: Proxy a -> Mode

-- promote the type to a value we can use for chaining actions
instance HasMode 'BrowseMail where
  mode _ = BrowseMail

instance HasMode 'SearchThreads where
  mode _ = SearchThreads

instance HasMode 'ViewMail where
  mode _ = ViewMail

instance HasMode 'ManageMailTags where
  mode _ = ManageMailTags

instance HasMode 'BrowseThreads where
  mode _ = BrowseThreads

instance HasMode 'Help where
  mode _ = Help

instance HasMode 'GatherHeadersFrom where
  mode _ = GatherHeadersFrom

instance HasMode 'GatherHeadersTo where
  mode _ = GatherHeadersTo

instance HasMode 'GatherHeadersSubject where
  mode _ = GatherHeadersSubject

instance HasMode 'ComposeEditor where
  mode _ = ComposeEditor

instance HasMode 'AddAttachment where
  mode _ = AddAttachment

instance HasMode 'ManageThreadTags where
  mode _ = ManageThreadTags

quit :: Action ctx (T.Next AppState)
quit = Action "quit the application" Brick.halt

continue :: Action ctx (T.Next AppState)
continue = Action "" Brick.continue

invokeEditor :: Action ctx (T.Next AppState)
invokeEditor = Action "invoke external editor" (Brick.suspendAndResume . liftIO . invokeEditor')

chain :: Action ctx AppState -> Action ctx a -> Action ctx a
chain (Action d1 f1) (Action d2 f2) =
  Action (if null d2 then d1 else d1 <> " and then " <> d2) (f1 >=> f2)

chain'
    :: forall ctx ctx' a.
       (HasMode ctx', ModeTransition ctx ctx')
    => Action ctx AppState
    -> Action ctx' a
    -> Action ctx a
chain' (Action d1 f1) (Action d2 f2) =
  Action (if null d2 then d1 else d1 <> " and then " <> d2) (f1 >=> switchMode >=> f2)
  where
    switchMode = pure . set asAppMode (mode (Proxy :: Proxy ctx'))

done :: forall a. Completable a => Action a AppState
done = Action "apply" (complete (Proxy :: Proxy a))

abort :: forall a. Resetable a => Action a AppState
abort = Action "cancel" (reset (Proxy :: Proxy a))

focus :: forall a. (HasMode a, Focusable a) => Action a AppState
focus = Action ("switch mode to " <> show (mode (Proxy :: Proxy a))) (switchFocus (Proxy :: Proxy a))

-- | A no-op action which just returns the current AppState
-- This action can be used at the start of an Action chain where an immediate
-- mode switch is required
noop :: Action ctx AppState
noop = Action "" pure

nextInput :: Action 'ComposeEditor AppState
nextInput =
    Action
        "focus next input"
        (pure . over (asCompose . cFocusFields) Brick.focusNext)

previousInput :: Action 'ComposeEditor AppState
previousInput =
    Action
        "focus previous input"
        (pure . over (asCompose . cFocusFields) Brick.focusPrev)

scrollUp :: forall ctx. (Scrollable ctx) => Action ctx AppState
scrollUp = Action
  { _aDescription = "scrolling up"
  , _aAction = \s -> Brick.vScrollPage (makeViewportScroller (Proxy :: Proxy ctx)) T.Up >> pure s
  }

scrollDown :: forall ctx. (Scrollable ctx) => Action ctx AppState
scrollDown = Action
  { _aDescription = "scrolling down"
  , _aAction = \s -> Brick.vScrollPage (makeViewportScroller (Proxy :: Proxy ctx)) T.Down >> pure s
  }

displayMail :: Action ctx AppState
displayMail =
    Action
    { _aDescription = "display an e-mail"
    , _aAction = \s -> liftIO $ updateStateWithParsedMail s
                       >>= updateReadState (RemoveTag $ view (asConfig . confNotmuch . nmNewTag) s)
    }

displayThreadMails :: Action 'BrowseThreads AppState
displayThreadMails =
    Action
    { _aDescription = "display an e-mail for threads"
    , _aAction = liftIO . setMailsForThread
    }

setUnread :: Action 'BrowseMail AppState
setUnread =
    Action
    { _aDescription = "toggle unread"
    , _aAction = \s -> liftIO $ updateReadState (AddTag $ view (asConfig . confNotmuch . nmNewTag) s) s
    }

listUp :: Action m AppState
listUp =
    Action
    { _aDescription = "mail index up one e-mail"
    , _aAction = \s -> case view asAppMode s of
        BrowseMail -> pure $ over (asMailIndex . miListOfMails) L.listMoveUp s
        ViewMail -> pure $ over (asMailIndex . miListOfMails) L.listMoveUp s
        AddAttachment -> pure $ over (asBrowseFiles . bfEntries) L.listMoveUp s
        _ -> pure $ over (asMailIndex . miListOfThreads) L.listMoveUp s
    }

listDown :: Action m AppState
listDown =
    Action
    { _aDescription = "mail index down one e-mail"
    , _aAction = \s -> case view asAppMode s of
        BrowseMail -> pure $ over (asMailIndex . miListOfMails) L.listMoveDown s
        ViewMail -> pure $ over (asMailIndex . miListOfMails) L.listMoveDown s
        AddAttachment -> pure $ over (asBrowseFiles . bfEntries) L.listMoveDown s
        _ -> pure $ over (asMailIndex. miListOfThreads) L.listMoveDown s
    }

switchComposeEditor :: Action 'BrowseThreads AppState
switchComposeEditor =
    Action
    { _aDescription = "switch to compose editor"
    , _aAction = \s -> case view (asCompose . cTmpFile) s of
                          Just _ -> pure $ set asAppMode ComposeEditor s
                          Nothing -> pure s
    }

replyMail :: Action 'BrowseMail AppState
replyMail =
    Action
    { _aDescription = "reply to an e-mail"
    , _aAction = replyToMail
    }

toggleHeaders :: Action 'ViewMail AppState
toggleHeaders = Action
  { _aDescription = "toggle mail headers"
  , _aAction = pure . go
  }
  where
    go :: AppState -> AppState
    go s = case view (asMailView . mvHeadersState) s of
      Filtered -> set (asMailView . mvHeadersState) ShowAll s
      ShowAll -> set (asMailView . mvHeadersState) Filtered s

setTags :: [TagOp] -> Action ctx AppState
setTags ops =
    Action
    { _aDescription = "apply given tags"
    , _aAction = \s ->
        case view asAppMode s of
          BrowseThreads -> selectedItemHelper (asMailIndex . miListOfThreads) s (manageThreadTags s ops)
          _ -> liftIO . selectedItemHelper (asMailIndex . miListOfMails) s $ \m ->
               either setError updateMails <$> applyTagOps ops (view vector [m]) s
    }

reloadList :: Action 'BrowseThreads AppState
reloadList = Action "reload list of threads" applySearch

selectNextUnread :: Action 'BrowseMail AppState
selectNextUnread =
  Action { _aDescription = "select next unread"
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

-- Function definitions for actions
--
findIndexWithOffset :: Int -> (a -> Bool) -> Vector.Vector a -> Maybe Int
findIndexWithOffset i fx = fmap (i+) . Vector.findIndex fx . Vector.drop i

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
            (\e -> setError e . set asAppMode BrowseMail)
            (\pmail -> set (asMailView . mvMail) (Just pmail) . set asAppMode ViewMail)
            <$> runExceptT (parseMail m (view (asConfig . confNotmuch . nmDatabase) s))

updateReadState :: TagOp -> AppState -> IO AppState
updateReadState op s =
  selectedItemHelper (asMailIndex . miListOfMails) s $ \m ->
  either setError updateMails <$> applyTagOps [op] (Vector.singleton m) s

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
    handleErr e = set asAppMode BrowseMail . setError e
    handleMail pmail =
      set (asCompose . cTo) (E.editor ComposeTo Nothing $ getFrom pmail)
      . set (asCompose . cFrom) (E.editor ComposeFrom Nothing $ getTo pmail)
      . set (asCompose . cSubject)
        (E.editor ComposeSubject Nothing ("Re: " <> getSubject pmail))
      . set asAppMode GatherHeadersFrom

sendMail :: AppState -> T.EventM Name AppState
sendMail s = do
    -- XXX if something has removed the tmpfile for whatever reason we go b00m :(
    body <- liftIO $ readFile (view (asCompose . cTmpFile) s ^?! _Just)
    let to =
            Address
                Nothing
                (unlines $ E.getEditContents $ view (asCompose . cTo) s)
    let from =
            Address
                Nothing
                (unlines $ E.getEditContents $ view (asCompose . cFrom) s)
    let m =
            simpleMail'
                to
                from
                (unlines $ E.getEditContents $ view (asCompose . cSubject) s)
                body
    liftIO $ renderSendMail m
    pure $ set asCompose initialCompose s

initialCompose :: Compose
initialCompose =
    Compose
        Nothing
        (E.editorText ComposeFrom (Just 1) "")
        (E.editorText ComposeTo (Just 1) "")
        (E.editorText ComposeSubject (Just 1) "")
        (Brick.focusRing [ComposeFrom, ComposeTo, ComposeSubject])


invokeEditor' :: AppState -> IO AppState
invokeEditor' s = do
  let editor = view (asConfig . confEditor) s
  tmpdir <- getTemporaryDirectory
  tmpfile <- emptyTempFile tmpdir "purebred.tmp"
  status <- onException (system (editor <> " " <> tmpfile)) (pure $ setError editorError)
  case status of
    ExitFailure _ -> pure $ s & set asAppMode BrowseMail & setError editorError
    ExitSuccess -> pure $ s & set (asCompose . cTmpFile) (Just tmpfile)

editorError :: Error
editorError = GenericError ("Editor command exited with error code."
  <> " Check your editor configuration and your terminal.")

emptyTempFile :: FilePath -> String -> IO FilePath
emptyTempFile targetDir template = bracket
  (openTempFile targetDir template)
  (\(_, handle) -> hClose handle)
  (\(filePath, _) -> pure filePath)

-- | Utility function to set tags on the notmuch thread.
-- Note, that the tags are not persisted since notmuch does not support writing
-- tags to the thread itself. It is currently only used to avoid reloaded the
-- thread from the database after tags have been written to each mail in the
-- thread.
--
tagThread
  :: [TagOp]
  -> NotmuchThread
  -> NotmuchThread
tagThread ops thread = foldr Notmuch.applyTagOp thread ops

manageThreadTags
    :: MonadIO m
    => AppState
    -> [TagOp]
    -> (t, NotmuchThread)
    -> m (AppState -> AppState)
manageThreadTags s ops t =
  let update ops' _ = over (asMailIndex . miListOfThreads) (L.listModify (tagThread ops'))
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
