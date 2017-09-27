{-# LANGUAGE OverloadedStrings #-}

module UI.Index.Keybindings where

import Brick.Main (continue, halt)
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Data.Vector (Vector)
import Control.Lens (over, set, view)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad ((>=>))
import Data.Text.Zipper (currentLine, gotoEOL)
import Data.Text (Text)
import qualified Graphics.Vty as V
import Storage.Notmuch (getMessages, addTag, removeTag, setNotmuchMailTags)
import Storage.ParsedMail (parseMail, getTo, getFrom, getSubject)
import Types
import Data.Monoid ((<>))
import Error

-- | Default Keybindings
indexKeybindings :: [Keybinding]
indexKeybindings =
    [ Keybinding "Quits the application" (V.EvKey V.KEsc []) halt
    , Keybinding
          "Manipulate the notmuch database query"
          (V.EvKey (V.KChar ':') [])
          focusSearch
    , Keybinding "display an e-mail" (V.EvKey V.KEnter []) displayMail
    , Keybinding "mail index down" (V.EvKey V.KDown []) mailIndexDown
    , Keybinding "mail index up" (V.EvKey V.KUp []) mailIndexUp
    , Keybinding "Switch between editor and main" (V.EvKey (V.KChar '\t') []) toggleComposeEditorAndMain
    , Keybinding "compose new mail" (V.EvKey (V.KChar 'm') []) composeMail
    , Keybinding "reply to mail" (V.EvKey (V.KChar 'r') []) replyMail
    , Keybinding "toggle unread" (V.EvKey (V.KChar 't') [])
      (liftIO . updateReadState addTag >=> continue)
    ]

indexsearchKeybindings :: [Keybinding]
indexsearchKeybindings =
    [ Keybinding "Cancel search" (V.EvKey V.KEsc []) cancelSearch
    , Keybinding "Apply search" (V.EvKey V.KEnter []) applySearchTerms
    ]

focusSearch :: AppState -> T.EventM Name (T.Next AppState)
focusSearch = continue
                . set (asMailIndex . miMode) SearchMail
                . over (asMailIndex . miSearchEditor) (E.applyEdit gotoEOL)

displayMail :: AppState -> T.EventM Name (T.Next AppState)
displayMail s = do
    s' <- liftIO $ updateStateWithParsedMail s >>= updateReadState removeTag
    continue s'

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

updateStateWithParsedMail :: AppState -> IO AppState
updateStateWithParsedMail s = ($ s) <$>
    case L.listSelectedElement (view (asMailIndex . miListOfMails) s) of
        Just (_,m) -> either
            (\e -> setError (GenericError e) . set asAppMode Main)
            (\pmail -> set (asMailView . mvMail) (Just pmail) . set asAppMode ViewMail)
            <$> liftIO (parseMail m)
        Nothing -> pure id

mailIndexEvent :: AppState -> (L.List Name NotmuchMail -> L.List Name NotmuchMail) -> T.EventM n (T.Next AppState)
mailIndexEvent s fx =
    continue $
    set
        (asMailIndex . miListOfMails)
        (fx $ view (asMailIndex . miListOfMails) s)
        s

mailIndexUp :: AppState -> T.EventM Name (T.Next AppState)
mailIndexUp s = mailIndexEvent s L.listMoveUp

mailIndexDown :: AppState -> T.EventM Name (T.Next AppState)
mailIndexDown s = mailIndexEvent s L.listMoveDown

composeMail :: AppState -> T.EventM Name (T.Next AppState)
composeMail s = continue $ set asAppMode GatherHeaders s

replyMail :: AppState -> T.EventM Name (T.Next AppState)
replyMail s =
  continue . ($ s)
  =<< case L.listSelectedElement (view (asMailIndex . miListOfMails) s) of
    Just (_, m) -> either handleErr handleMail <$> liftIO (parseMail m)
    Nothing -> pure id
  where
    handleErr e = set asAppMode Main . setError (GenericError e)
    handleMail pmail =
      set (asCompose . cTo) (E.editor GatherHeadersTo Nothing $ getFrom pmail)
      . set (asCompose . cFrom) (E.editor GatherHeadersFrom Nothing $ getTo pmail)
      . set (asCompose . cSubject)
        (E.editor GatherHeadersSubject Nothing ("Re: " <> getSubject pmail))
      . set (asCompose . cFocus) AskFrom
      . set asAppMode GatherHeaders

toggleComposeEditorAndMain :: AppState -> T.EventM Name (T.Next AppState)
toggleComposeEditorAndMain s =
    case view (asCompose . cTmpFile) s of
        Just _ -> continue $ set asAppMode ComposeEditor s
        Nothing -> continue s

cancelSearch  :: AppState -> T.EventM Name (T.Next AppState)
cancelSearch s = continue $ set (asMailIndex . miMode) BrowseMail s

setError :: Error -> AppState -> AppState
setError = set asError . Just

applySearchTerms :: AppState -> T.EventM Name (T.Next AppState)
applySearchTerms s =
   runExceptT (getMessages searchterms (view (asConfig . confNotmuch) s))
   >>= continue . ($ s) . either setError reloadListOfMails
     where searchterms = currentLine $ view (asMailIndex . miSearchEditor . E.editContentsL) s

reloadListOfMails :: Vector NotmuchMail -> AppState -> AppState
reloadListOfMails vec =
  set (asMailIndex . miListOfMails) (L.list ListOfMails vec 1)
  . set asAppMode Main
  . set (asMailIndex . miMode) BrowseMail
