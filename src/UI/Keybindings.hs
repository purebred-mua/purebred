{-# LANGUAGE OverloadedStrings #-}
module UI.Keybindings where

import qualified Brick.Main                as M
import qualified Brick.Types               as T
import qualified Brick.Widgets.Edit        as E
import qualified Brick.Widgets.List        as L
import           Control.Lens.Fold         ((^?!))
import           Control.Lens.Getter       ((^.))
import           Control.Lens.Lens         ((&))
import           Control.Lens.Prism        (_Just)
import           Control.Lens.Setter       ((.~), (?~))
import           Control.Monad.IO.Class    (liftIO)
import           Data.List                 (find)
import           Data.Text                 (unlines, unpack)
import           Data.Text.Lazy.IO         (readFile)
import           Data.Text.Zipper          (currentLine)
import qualified Graphics.Vty              as V
import           Graphics.Vty.Input.Events (Event)
import           Network.Mail.Mime         (Address (..), renderSendMail,
                                            simpleMail')
import           Prelude                   hiding (readFile, unlines)
import           Storage.Mail              (Mail)
import           Storage.Notmuch           (getMessages)
import           Storage.ParsedMail        (parseMail)
import           UI.Types


-- | A generic event handler using Keybindings by default if available
handleEvent
    :: [Keybinding]  -- ^ Keybindings to lookup
    -> (AppState -> Event -> T.EventM Name (T.Next AppState))  -- ^ default handler if no keybinding matches
    -> AppState
    -> T.BrickEvent Name e
    -> T.EventM Name (T.Next AppState)
handleEvent kbs def s (T.VtyEvent ev) = case lookupKeybinding ev kbs of
  Just kb -> kb^.kbAction $ s
  Nothing -> def s ev
handleEvent _ _ s _ = M.continue s

lookupKeybinding :: Event -> [Keybinding] -> Maybe Keybinding
lookupKeybinding e = find (\x -> x^.kbEvent == e)

-- | Default Keybindings
indexKeybindings :: [Keybinding]
indexKeybindings =
    [ Keybinding "Quits the application" (V.EvKey V.KEsc []) M.halt
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

displayMailKeybindings :: [Keybinding]
displayMailKeybindings =
  [ Keybinding "Return to list of mails" (V.EvKey V.KEsc []) (\s -> M.continue $ asAppMode .~ Main $ s)
  , Keybinding "Scroll e-mail up" (V.EvKey V.KBS []) (\s -> scrollMailViewPage s T.Up)
  , Keybinding "Scroll e-mail down" (V.EvKey (V.KChar ' ') []) (\s -> scrollMailViewPage s T.Down)
  ]

interactiveGatherHeadersKeybindings :: [Keybinding]
interactiveGatherHeadersKeybindings =
    [Keybinding "Return to list of mails" (V.EvKey V.KEsc []) cancelToMain]

composeEditorKeybindings :: [Keybinding]
composeEditorKeybindings =
    [ Keybinding "Toggle index view" (V.EvKey (V.KChar '\t') []) cancelToMain
    , Keybinding "Send e-mail" (V.EvKey (V.KChar 'y') []) sendMail
    , Keybinding
          "Cancel compose"
          (V.EvKey V.KEsc [])
          (\s ->
                M.continue $ resetCompose s)]

toggleComposeEditorAndMain :: AppState -> T.EventM Name (T.Next AppState)
toggleComposeEditorAndMain s =
    case s ^. asCompose ^. cTmpFile of
        Just _ -> M.continue $ s & asAppMode .~ ComposeEditor
        Nothing -> M.continue s

cancelToMain :: AppState -> T.EventM Name (T.Next AppState)
cancelToMain s = M.continue $ asAppMode .~ Main $ s

mailIndexEvent :: AppState -> (L.List Name Mail -> L.List Name Mail) -> T.EventM n (T.Next AppState)
mailIndexEvent s fx =
    M.continue $ s & asMailIndex . miListOfMails .~
    (fx $ s ^. asMailIndex ^. miListOfMails)

mailIndexUp :: AppState -> T.EventM Name (T.Next AppState)
mailIndexUp s = mailIndexEvent s L.listMoveUp

mailIndexDown :: AppState -> T.EventM Name (T.Next AppState)
mailIndexDown s = mailIndexEvent s L.listMoveDown

focusSearch :: AppState -> T.EventM Name (T.Next AppState)
focusSearch s = M.continue $ asMailIndex . miMode .~ SearchMail $ s

displayMail :: AppState -> T.EventM Name (T.Next AppState)
displayMail s = do
    s' <- liftIO $ updateStateWithParsedMail s
    M.continue $ s'

composeMail :: AppState -> T.EventM Name (T.Next AppState)
composeMail s = M.continue $ asAppMode .~ GatherHeaders $ s

cancelSearch  :: AppState -> T.EventM Name (T.Next AppState)
cancelSearch s = M.continue $ asMailIndex . miMode .~ BrowseMail $ s

applySearchTerms :: AppState -> T.EventM Name (T.Next AppState)
applySearchTerms s = do
    let searchterms =
            currentLine $ s ^. asMailIndex ^. miSearchEditor ^. E.editContentsL
    vec <- liftIO $ getMessages (s ^. asNotmuchDatabaseFp) (unpack searchterms)
    let listWidget = (L.list ListOfMails vec 1)
    M.continue $ s & asMailIndex . miListOfMails .~ listWidget & asAppMode .~
        Main

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

scrollMailViewPage :: AppState -> T.Direction -> T.EventM Name (T.Next AppState)
scrollMailViewPage s d = do
  let vp = M.viewportScroll ScrollingMailView
  M.vScrollPage vp d
  M.continue s

sendMail :: AppState -> T.EventM Name (T.Next AppState)
sendMail s = do
    body <- liftIO $ readFile (s ^. asCompose ^. cTmpFile ^?! _Just)  -- XXX if something has removed the tmpfile for whatever reason we go b00m :(
    let to =
            Address
                Nothing
                (unlines $ E.getEditContents $ s ^. asCompose ^. cTo)
    let from =
            Address
                Nothing
                (unlines $ E.getEditContents $ s ^. asCompose ^. cFrom)
    let m =
            simpleMail'
                to
                from
                (unlines $ E.getEditContents $ s ^. asCompose ^. cSubject)
                body
    liftIO $ renderSendMail m
    M.continue $ s & asCompose .~ initialCompose & asAppMode .~ Main

resetCompose :: AppState -> AppState
resetCompose s = s & asCompose .~ initialCompose & asAppMode .~ Main

initialCompose :: Compose
initialCompose =
    Compose
        Nothing
        AskFrom
        (E.editor GatherHeadersFrom Nothing "")
        (E.editor GatherHeadersTo Nothing "")
        (E.editor GatherHeadersSubject Nothing "")
