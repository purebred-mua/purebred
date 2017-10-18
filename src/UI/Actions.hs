{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Actions (
  Scrollable(..)
  , backToIndex
  , quit
  , focus
  , done
  , abort
  , displayMail
  , setUnread
  , mailIndexUp
  , mailIndexDown
  , switchComposeEditor
  , composeMail
  , replyMail
  , scrollUp
  , scrollDown
  , toggleHeaders
  , initialCompose
  , continue
  , chain
  , viewHelp
  ) where

import qualified Brick.Main as Brick
       (continue, halt, vScrollPage, viewportScroll, ViewportScroll)
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Network.Mail.Mime (Address(..), renderSendMail, simpleMail')
import Data.Proxy
import Data.Semigroup ((<>))
import Data.Vector (Vector)
import Data.Text (unlines)
import Data.Text.Lazy.IO (readFile)
import Prelude hiding (readFile, unlines)
import Control.Lens (set, over, view, _Just)
import Control.Lens.Fold ((^?!))
import Control.Monad ((>=>))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Zipper (currentLine, gotoEOL)
import Data.Text (Text)
import Storage.Notmuch (getMessages, addTag, removeTag, setNotmuchMailTags)
import Storage.ParsedMail (parseMail, getTo, getFrom, getSubject)
import Types
import Error

class Scrollable (n :: Mode) where
  makeViewportScroller :: Proxy n -> Brick.ViewportScroll Name

instance Scrollable 'ViewMail where
  makeViewportScroller _ = Brick.viewportScroll ScrollingMailView

instance Scrollable 'Help where
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
class Completable (m :: Mode) where
  complete :: Proxy m -> AppState -> T.EventM Name AppState

instance Completable 'SearchMail where
  complete _ = applySearch

instance Completable 'ComposeEditor where
  complete _ = sendMail


-- | Generalisation of reset actions, whether they reset editors back to their
-- initial state or throw away composed, but not yet sent mails.
--
class Resetable (m :: Mode) where
  reset :: Proxy m -> AppState -> T.EventM Name AppState

instance Resetable 'ComposeEditor where
  reset _ = pure . set asCompose initialCompose

-- | Generalisation of focus changes between widgets on the same "view"
-- expressed with the mode in the application state.
--
class Focusable (m :: Mode) where
  switchFocus :: Proxy m -> AppState -> T.EventM Name AppState

instance Focusable 'BrowseMail where
  switchFocus _ = pure
                  . set asAppMode SearchMail
                  . over (asMailIndex . miSearchEditor) (E.applyEdit gotoEOL)

quit :: Action ctx (T.Next AppState)
quit = Action "quit the application" Brick.halt

continue :: Action ctx (T.Next AppState)
continue = Action "" Brick.continue

chain :: Action ctx AppState -> Action ctx a -> Action ctx a
chain (Action d1 f1) (Action d2 f2) =
  Action (if null d2 then d1 else d1 <> " and then " <> d2) (f1 >=> f2)

done :: forall a. Completable a => Action a AppState
done = Action "apply" (complete (Proxy :: Proxy a))

abort :: forall a. Resetable a => Action a AppState
abort = Action "cancel" (reset (Proxy :: Proxy a))

focus :: forall a. Focusable a => Action a AppState
focus = Action "switch focus" (switchFocus (Proxy :: Proxy a))

backToIndex :: Action ctx AppState
backToIndex =
    Action
    { _aDescription = "back to the index"
    , _aAction = pure . set asAppMode BrowseMail
    }

viewHelp :: Action ctx AppState
viewHelp = Action "view all key bindings" (pure . set asAppMode Help)

scrollUp :: forall ctx. (Scrollable ctx) => Action ctx AppState
scrollUp = Action
  { _aDescription = "scrolling up"
  , _aAction = (\s -> Brick.vScrollPage (makeViewportScroller (Proxy :: Proxy ctx)) T.Up >> pure s)
  }

scrollDown :: forall ctx. (Scrollable ctx) => Action ctx AppState
scrollDown = Action
  { _aDescription = "scrolling down"
  , _aAction = (\s -> Brick.vScrollPage (makeViewportScroller (Proxy :: Proxy ctx)) T.Down >> pure s)
  }

composeMail :: Action 'BrowseMail AppState
composeMail =
    Action
    { _aDescription = "compose a new mail"
    , _aAction = pure . set asAppMode GatherHeaders
    }

displayMail :: Action 'BrowseMail AppState
displayMail =
    Action
    { _aDescription = "display an e-mail"
    , _aAction = \s -> liftIO $ updateStateWithParsedMail s >>= updateReadState removeTag
    }

setUnread :: Action 'BrowseMail AppState
setUnread =
    Action
    { _aDescription = "toggle unread"
    , _aAction = (liftIO . updateReadState addTag)
    }

mailIndexUp :: Action 'BrowseMail AppState
mailIndexUp =
    Action
    { _aDescription = "mail index up one e-mail"
    , _aAction = mailIndexEvent L.listMoveUp
    }

mailIndexDown :: Action 'BrowseMail AppState
mailIndexDown =
    Action
    { _aDescription = "mail index down one e-mail"
    , _aAction = mailIndexEvent L.listMoveDown
    }

switchComposeEditor :: Action 'BrowseMail AppState
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

-- Function definitions for actions
--
applySearch :: AppState -> T.EventM Name AppState
applySearch s =
   runExceptT (getMessages searchterms (view (asConfig . confNotmuch) s))
   >>= pure . ($ s) . either setError reloadListOfMails
     where searchterms = currentLine $ view (asMailIndex . miSearchEditor . E.editContentsL) s

updateStateWithParsedMail :: AppState -> IO AppState
updateStateWithParsedMail s = ($ s) <$>
    case L.listSelectedElement (view (asMailIndex . miListOfMails) s) of
        Just (_,m) -> either
            (\e -> setError e . set asAppMode BrowseMail)
            (\pmail -> set (asMailView . mvMail) (Just pmail) . set asAppMode ViewMail)
            <$> runExceptT (parseMail m (view (asConfig . confNotmuch . nmDatabase) s))
        Nothing -> pure id

updateReadState :: (NotmuchMail -> Text -> NotmuchMail) -> AppState -> IO AppState
updateReadState op s =
    ($ s) <$> case L.listSelectedElement (view (asMailIndex . miListOfMails) s) of
        Just (_,m) ->
            let newTag = view (asConfig . confNotmuch . nmNewTag) s
                dbpath = view (asConfig . confNotmuch . nmDatabase) s
            in either setError updateMailInList
               <$> runExceptT (setNotmuchMailTags dbpath (op m newTag))
        Nothing -> pure $ setError (GenericError "No mail selected to update tags")

updateMailInList :: NotmuchMail -> AppState -> AppState
updateMailInList m s =
    let l = L.listModify (const m) (view (asMailIndex . miListOfMails) s)
    in set (asMailIndex . miListOfMails) l s

setError :: Error -> AppState -> AppState
setError = set asError . Just

reloadListOfMails :: Vector NotmuchMail -> AppState -> AppState
reloadListOfMails vec =
  set (asMailIndex . miListOfMails) (L.list ListOfMails vec 1)
  . set asAppMode BrowseMail

mailIndexEvent
    :: (L.List Name NotmuchMail -> L.List Name NotmuchMail)
    -> AppState
    -> T.EventM n AppState
mailIndexEvent fx s =
    pure $
    set
        (asMailIndex . miListOfMails)
        (fx $ view (asMailIndex . miListOfMails) s)
        s

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
      set (asCompose . cTo) (E.editor GatherHeadersTo Nothing $ getFrom pmail)
      . set (asCompose . cFrom) (E.editor GatherHeadersFrom Nothing $ getTo pmail)
      . set (asCompose . cSubject)
        (E.editor GatherHeadersSubject Nothing ("Re: " <> getSubject pmail))
      . set (asCompose . cFocus) AskFrom
      . set asAppMode GatherHeaders

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
        AskFrom
        (E.editor GatherHeadersFrom Nothing "")
        (E.editor GatherHeadersTo Nothing "")
        (E.editor GatherHeadersSubject Nothing "")
