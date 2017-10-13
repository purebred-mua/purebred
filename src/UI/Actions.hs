{-# LANGUAGE OverloadedStrings #-}
module UI.Actions
       (backToIndex, quit, focusSearch, displayMail, setUnread,
        applySearchTerms, mailIndexUp, mailIndexDown, switchComposeEditor,
        composeMail, replyMail, scrollUp, scrollDown, toggleHeaders, send,
        reset, initialCompose, continue, chain)
       where

import qualified Brick.Main as B (continue, halt, vScrollPage)
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Network.Mail.Mime (Address(..), renderSendMail, simpleMail')
import Data.Semigroup ((<>))
import Data.Vector (Vector)
import Data.Text (unlines)
import Data.Text.Lazy.IO (readFile)
import Prelude hiding (readFile, unlines)
import Control.Lens (set, over, view, (&), _Just)
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

quit :: Action ctx (T.Next AppState)
quit = Action "quit the application" B.halt

continue :: Action ctx (T.Next AppState)
continue = Action "" B.continue

chain :: Action ctx AppState -> Action ctx a -> Action ctx a
chain (Action d1 f1) (Action d2 f2) =
  Action (if null d2 then d1 else d1 <> " and then " <> d2) (f1 >=> f2)

backToIndex :: Action ctx AppState
backToIndex =
    Action
    { _aDescription = "back to the index"
    , _aAction = pure . set asAppMode BrowseMail
    }

composeMail :: Action (L.List Name NotmuchMail) AppState
composeMail =
    Action
    { _aDescription = "compose a new mail"
    , _aAction = pure . set asAppMode GatherHeaders
    }

focusSearch :: Action (L.List Name NotmuchMail) AppState
focusSearch =
    Action
    { _aDescription = "Manipulate the notmuch database query"
    , _aAction = (pure
                  . set asAppMode SearchMail
                  . over (asMailIndex . miSearchEditor) (E.applyEdit gotoEOL))
    }

displayMail :: Action (L.List Name NotmuchMail) AppState
displayMail =
    Action
    { _aDescription = "display an e-mail"
    , _aAction = \s -> liftIO $ updateStateWithParsedMail s >>= updateReadState removeTag
    }

setUnread :: Action (L.List Name NotmuchMail) AppState
setUnread =
    Action
    { _aDescription = "toggle unread"
    , _aAction = (liftIO . updateReadState addTag)
    }

applySearchTerms :: Action (E.Editor Text Name) AppState
applySearchTerms =
    Action
    { _aDescription = "apply search"
    , _aAction = applySearch
    }

mailIndexUp :: Action (L.List Name NotmuchMail) AppState
mailIndexUp =
    Action
    { _aDescription = "mail index up one e-mail"
    , _aAction = mailIndexEvent L.listMoveUp
    }

mailIndexDown :: Action (L.List Name NotmuchMail) AppState
mailIndexDown =
    Action
    { _aDescription = "mail index down one e-mail"
    , _aAction = mailIndexEvent L.listMoveDown
    }

switchComposeEditor :: Action (L.List Name NotmuchMail) AppState
switchComposeEditor =
    Action
    { _aDescription = "switch to compose editor"
    , _aAction = \s -> case view (asCompose . cTmpFile) s of
                          Just _ -> pure $ set asAppMode ComposeEditor s
                          Nothing -> pure s
    }

replyMail :: Action (L.List Name NotmuchMail) AppState
replyMail =
    Action
    { _aDescription = "reply to an e-mail"
    , _aAction = replyToMail
    }

scrollUp :: Scrollable ctx => Action (T.Widget ctx) AppState
scrollUp = Action
  { _aDescription = "scrolling up"
  , _aAction = (\s -> B.vScrollPage (makeViewportScroller s) T.Up >> pure s)
  }
  
scrollDown :: Scrollable ctx => Action (T.Widget ctx) AppState
scrollDown = Action
  { _aDescription = "scrolling down"
  , _aAction = (\s -> B.vScrollPage (makeViewportScroller s) T.Down >> pure s)
  }

toggleHeaders :: Action (T.Widget Name) AppState
toggleHeaders = Action
  { _aDescription = "toggle mail headers"
  , _aAction = pure . go
  }
  where
    go :: AppState -> AppState
    go s = case view (asMailView . mvHeadersState) s of
      Filtered -> set (asMailView . mvHeadersState) ShowAll s
      ShowAll -> set (asMailView . mvHeadersState) Filtered s

send :: Action (E.Editor Text Name) AppState
send = Action
  { _aDescription = "send mail"
  , _aAction = sendMail
  }

reset :: Action (E.Editor Text Name) AppState
reset = Action
  { _aDescription = "cancel compose"
  , _aAction = pure . set asCompose initialCompose . set asAppMode BrowseMail
  }

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
    pure $ set asCompose initialCompose s & set asAppMode BrowseMail

initialCompose :: Compose
initialCompose =
    Compose
        Nothing
        AskFrom
        (E.editor GatherHeadersFrom Nothing "")
        (E.editor GatherHeadersTo Nothing "")
        (E.editor GatherHeadersSubject Nothing "")
