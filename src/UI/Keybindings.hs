module UI.Keybindings where

import qualified Brick.Main                as M
import qualified Brick.Types               as T
import qualified Brick.Widgets.Edit        as E
import qualified Brick.Widgets.List        as L
import           Control.Lens.Getter       ((^.))
import           Control.Lens.Lens         ((&))
import           Control.Lens.Setter       ((.~), (?~))
import           Control.Monad.IO.Class    (liftIO)
import           Data.List                 (find)
import           Data.Text                 (unpack)
import           Data.Text.Zipper          (currentLine)
import qualified Graphics.Vty              as V
import           Graphics.Vty.Input.Events (Event)
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
    , Keybinding "mail index up" (V.EvKey V.KUp []) mailIndexUp]

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
