{-# LANGUAGE OverloadedStrings #-}
module UI.Actions
       (backToIndex, haltApp, focusSearch, displayMail, setUnread,
        applySearchTerms, mailIndexUp, mailIndexDown, switchComposeEditor,
        composeMail, replyMail, scrollUp, scrollDown, toggleHeaders, send,
        reset, updateStateWithParsedMail, updateReadState, initialCompose)
       where

import Brick.Main (continue, halt, vScrollPage)
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

backToIndex :: Action a
backToIndex =
    Action
    { _aDescription = "back to the index"
    , _aAction = continue . set asAppMode BrowseMail
    }

haltApp :: Action a
haltApp =
    Action
    { _aDescription = "quit the application"
    , _aAction = halt
    }

composeMail :: Action (L.List Name NotmuchMail)
composeMail =
    Action
    { _aDescription = "compose a new mail"
    , _aAction = continue . set asAppMode GatherHeaders
    }

focusSearch :: Action (L.List Name NotmuchMail)
focusSearch =
    Action
    { _aDescription = "Manipulate the notmuch database query"
    , _aAction = (continue
                   . set asAppMode SearchMail
                   . over (asMailIndex . miSearchEditor) (E.applyEdit gotoEOL))
    }

displayMail :: Action (L.List Name NotmuchMail)
displayMail =
    Action
    { _aDescription = "display an e-mail"
    , _aAction = \s ->
                      do s' <-
                             liftIO $
                             updateStateWithParsedMail s >>=
                             updateReadState removeTag
                         continue s'
    }

setUnread :: Action (L.List Name NotmuchMail)
setUnread =
    Action
    { _aDescription = "toggle unread"
    , _aAction = (liftIO . updateReadState addTag >=> continue)
    }

applySearchTerms :: Action (E.Editor Text Name)
applySearchTerms =
    Action
    { _aDescription = "apply search"
    , _aAction = applySearch
    }

mailIndexUp :: Action (L.List Name NotmuchMail)
mailIndexUp =
    Action
    { _aDescription = "mail index up one e-mail"
    , _aAction = mailIndexEvent L.listMoveUp
    }

mailIndexDown :: Action (L.List Name NotmuchMail)
mailIndexDown =
    Action
    { _aDescription = "mail index down one e-mail"
    , _aAction = mailIndexEvent L.listMoveDown
    }

switchComposeEditor :: Action (L.List Name NotmuchMail)
switchComposeEditor =
    Action
    { _aDescription = "switch to compose editor"
    , _aAction = \s -> case view (asCompose . cTmpFile) s of
                          Just _ -> continue $ set asAppMode ComposeEditor s
                          Nothing -> continue s
    }

replyMail :: Action (L.List Name NotmuchMail)
replyMail =
    Action
    { _aDescription = "reply to an e-mail"
    , _aAction = replyToMail
    }

scrollUp :: Scrollable a => Action (T.Widget a)
scrollUp = Action
  { _aDescription = "scrolling up"
  , _aAction = (\s -> vScrollPage (makeViewportScroller s) T.Up >> continue s)
  }

scrollDown :: Scrollable a => Action (T.Widget a)
scrollDown = Action
  { _aDescription = "scrolling down"
  , _aAction = (\s -> vScrollPage (makeViewportScroller s) T.Down >> continue s)
  }

toggleHeaders :: Action (T.Widget Name)
toggleHeaders = Action
  { _aDescription = "toggle mail headers"
  , _aAction = (continue . go)
  }
  where
    go :: AppState -> AppState
    go s = case view (asMailView . mvHeadersState) s of
      Filtered -> set (asMailView . mvHeadersState) ShowAll s
      ShowAll -> set (asMailView . mvHeadersState) Filtered s

send :: Action (E.Editor Text Name)
send = Action
  { _aDescription = "send mail"
  , _aAction = sendMail
  }

reset :: Action (E.Editor Text Name)
reset = Action
  { _aDescription = "cancel compose"
  , _aAction = continue . set asCompose initialCompose . set asAppMode BrowseMail
  }

applySearch :: AppState -> T.EventM Name (T.Next AppState)
applySearch s =
   runExceptT (getMessages searchterms (view (asConfig . confNotmuch) s))
   >>= continue . ($ s) . either setError reloadListOfMails
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
    -> T.EventM n (T.Next AppState)
mailIndexEvent fx s =
    continue $
    set
        (asMailIndex . miListOfMails)
        (fx $ view (asMailIndex . miListOfMails) s)
        s

replyToMail :: AppState -> T.EventM Name (T.Next AppState)
replyToMail s =
  continue . ($ s)
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

sendMail :: AppState -> T.EventM Name (T.Next AppState)
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
    continue $ set asCompose initialCompose s & set asAppMode BrowseMail

initialCompose :: Compose
initialCompose =
    Compose
        Nothing
        AskFrom
        (E.editor GatherHeadersFrom Nothing "")
        (E.editor GatherHeadersTo Nothing "")
        (E.editor GatherHeadersSubject Nothing "")
