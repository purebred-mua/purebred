{-# LANGUAGE OverloadedStrings #-}
module UI.Index.Keybindings where

import Brick.Main (continue, halt)
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Data.Text.Zipper (gotoEOL)
import Control.Lens.Getter (view)
import Control.Lens.Lens ((&))
import Control.Lens.Setter ((?~), set, over)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Zipper (currentLine)
import qualified Graphics.Vty as V
import Storage.Notmuch (getMessages)
import Storage.ParsedMail (parseMail, getTo, getFrom, getSubject)
import Types
import Data.Monoid ((<>))

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
    ]

indexsearchKeybindings :: [Keybinding]
indexsearchKeybindings =
    [ Keybinding "Cancel search" (V.EvKey V.KEsc []) cancelSearch
    , Keybinding "Apply search" (V.EvKey V.KEnter []) applySearchTerms
    ]

focusSearch :: AppState -> T.EventM Name (T.Next AppState)
focusSearch s = continue $ s
                & set (asMailIndex . miMode) SearchMail
                & over (asMailIndex . miSearchEditor) (E.applyEdit gotoEOL)

displayMail :: AppState -> T.EventM Name (T.Next AppState)
displayMail s = do
    s' <- liftIO $ updateStateWithParsedMail s
    continue $ s'

updateStateWithParsedMail :: AppState -> IO AppState
updateStateWithParsedMail s =
    case L.listSelectedElement (view (asMailIndex . miListOfMails) s) of
        Just (_,m) -> do
            parsed <- parseMail m
            case parsed of
                Left e -> pure $ s & asError ?~ e & set asAppMode Main
                Right pmail ->
                    pure $
                    set (asMailView . mvMail) (Just pmail) s &
                    set asAppMode ViewMail
        Nothing -> pure s

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
replyMail s = case L.listSelectedElement (view (asMailIndex . miListOfMails) s) of
  Just (_, m) -> do
    parsed <- liftIO $ parseMail m
    case parsed of
      Left e -> continue $ s & asError ?~ e & set asAppMode Main
      Right pmail -> do
        let s' = set (asCompose . cTo) (E.editor GatherHeadersTo Nothing $ getFrom pmail) s &
                 set (asCompose . cFrom) (E.editor GatherHeadersFrom Nothing $ getTo pmail) &
                 set (asCompose . cSubject) (E.editor GatherHeadersSubject Nothing $ ("Re: " <> getSubject pmail)) &
                 set (asCompose . cFocus) AskFrom &
                 set asAppMode GatherHeaders
        continue s'
  Nothing -> continue s

toggleComposeEditorAndMain :: AppState -> T.EventM Name (T.Next AppState)
toggleComposeEditorAndMain s =
    case view (asCompose . cTmpFile) s of
        Just _ -> continue $ set asAppMode ComposeEditor s
        Nothing -> continue s

cancelSearch  :: AppState -> T.EventM Name (T.Next AppState)
cancelSearch s = continue $ set (asMailIndex . miMode) BrowseMail s

applySearchTerms :: AppState -> T.EventM Name (T.Next AppState)
applySearchTerms s = do
    let searchterms =
            currentLine $
            view (asMailIndex . miSearchEditor . E.editContentsL) s
    vec <- liftIO $ getMessages searchterms (view (asConfig . confNotmuch) s)
    let listWidget = (L.list ListOfMails vec 1)
    continue $
        set (asMailIndex . miListOfMails) listWidget s & set asAppMode Main &
        set (asMailIndex . miMode) BrowseMail
