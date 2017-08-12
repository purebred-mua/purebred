{-# LANGUAGE OverloadedStrings #-}
module UI.Keybindings where

import qualified Brick.Main                as M
import qualified Brick.Types               as T
import qualified Brick.Widgets.Edit        as E
import qualified Brick.Widgets.List        as L
import           Control.Lens.Getter       ((^.))
import           Control.Lens.Lens         ((&))
import           Control.Lens.Setter       ((.~))
import           Data.List                 (find)
import           Graphics.Vty.Input.Events (Event)
import           Prelude                   hiding (readFile, unlines)
import           Storage.Mail              (Mail)
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

initialCompose :: Compose
initialCompose =
    Compose
        Nothing
        AskFrom
        (E.editor GatherHeadersFrom Nothing "")
        (E.editor GatherHeadersTo Nothing "")
        (E.editor GatherHeadersSubject Nothing "")
