module UI.Index.Keybindings where

import           Brick.Main             (continue, halt)
import qualified Brick.Types            as T
import qualified Brick.Widgets.Edit     as E
import qualified Brick.Widgets.List     as L
import           Control.Lens.Getter    ((^.))
import           Control.Lens.Lens      ((&))
import           Control.Lens.Setter    ((.~), (?~))
import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (unpack)
import           Data.Text.Zipper       (currentLine)
import qualified Graphics.Vty           as V
import           Storage.Mail           (Mail)
import           Storage.Notmuch        (getMessages)
import           Storage.ParsedMail     (parseMail)
import           UI.Types

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
    , Keybinding "compose new mail" (V.EvKey (V.KChar 'm') []) composeMail]

indexsearchKeybindings :: [Keybinding]
indexsearchKeybindings =
    [ Keybinding "Cancel search" (V.EvKey V.KEsc []) cancelSearch
    , Keybinding "Apply search" (V.EvKey V.KEnter []) applySearchTerms
    ]

focusSearch :: AppState -> T.EventM Name (T.Next AppState)
focusSearch s = continue $ asMailIndex . miMode .~ SearchMail $ s

displayMail :: AppState -> T.EventM Name (T.Next AppState)
displayMail s = do
    s' <- liftIO $ updateStateWithParsedMail s
    continue $ s'

updateStateWithParsedMail :: AppState -> IO AppState
updateStateWithParsedMail s =
    case L.listSelectedElement (s ^. asMailIndex ^. miListOfMails) of
        Just (_,m) -> do
            parsed <- parseMail m
            case parsed of
                Left e -> pure $ s & asError ?~ e & asAppMode .~ Main
                Right pmail ->
                    pure $
                    s & asMailView .~ MailView (Just pmail) & asAppMode .~
                    ViewMail
        Nothing -> pure s

mailIndexEvent :: AppState -> (L.List Name Mail -> L.List Name Mail) -> T.EventM n (T.Next AppState)
mailIndexEvent s fx =
    continue $ s & asMailIndex . miListOfMails .~
    (fx $ s ^. asMailIndex ^. miListOfMails)

mailIndexUp :: AppState -> T.EventM Name (T.Next AppState)
mailIndexUp s = mailIndexEvent s L.listMoveUp

mailIndexDown :: AppState -> T.EventM Name (T.Next AppState)
mailIndexDown s = mailIndexEvent s L.listMoveDown

composeMail :: AppState -> T.EventM Name (T.Next AppState)
composeMail s = continue $ asAppMode .~ GatherHeaders $ s

toggleComposeEditorAndMain :: AppState -> T.EventM Name (T.Next AppState)
toggleComposeEditorAndMain s =
    case s ^. asCompose ^. cTmpFile of
        Just _ -> continue $ s & asAppMode .~ ComposeEditor
        Nothing -> continue s

cancelSearch  :: AppState -> T.EventM Name (T.Next AppState)
cancelSearch s = continue $ asMailIndex . miMode .~ BrowseMail $ s

applySearchTerms :: AppState -> T.EventM Name (T.Next AppState)
applySearchTerms s = do
    let searchterms =
            currentLine $ s ^. asMailIndex ^. miSearchEditor ^. E.editContentsL
    vec <-
        liftIO $
        getMessages
            (s ^. asConfig ^. confNotmuchDatabase)
            (unpack searchterms)
    let listWidget = (L.list ListOfMails vec 1)
    continue $ s & asMailIndex . miListOfMails .~ listWidget & asAppMode .~
        Main
