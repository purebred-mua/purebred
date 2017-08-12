{-# LANGUAGE OverloadedStrings #-}
module UI.Index.Main where

import Brick.AttrMap (AttrName)
import Data.Monoid ((<>))
import qualified Brick.Main                as M
import           Brick.Types               (Padding (..), Widget)
import qualified Brick.Types               as T
import Brick.Widgets.Core
       (hLimit, padLeft, txt, vBox, vLimit, withAttr, (<+>))
import qualified Brick.Widgets.Edit        as E
import qualified Brick.Widgets.List        as L
import           Control.Lens.Getter       ((^.), view)
import           Control.Lens.Lens         ((&))
import           Control.Lens.Setter       ((.~))
import           Graphics.Vty.Input.Events (Event)
import           Storage.Mail
import           UI.Draw.Main              (editorDrawContent, fillLine)
import           UI.Keybindings            (handleEvent)
import           UI.Status.Main            (statusbar)
import           UI.Types

drawMain :: AppState -> [Widget Name]
drawMain s = [ui]
  where
    editorFocus = (view (asMailIndex . miMode) s == SearchMail)
    inputBox = E.renderEditor editorDrawContent editorFocus (view (asMailIndex . miSearchEditor) s)
    ui = vBox [renderMailList s, statusbar s, vLimit 1 inputBox]

renderMailList :: AppState -> Widget Name
renderMailList s = let listFocus = view (asMailIndex . miMode) s == BrowseMail
                   in L.renderList listDrawElement listFocus (view (asMailIndex . miListOfMails) s)

listDrawElement :: Bool -> Mail -> Widget Name
listDrawElement sel a =
    let selected w = if sel then withAttr L.listSelectedAttr w else w
        newMail m w = if (view mailIsNew m) then withAttr listNewMailAttr w else w
        widget = padLeft (Pad 1) $ hLimit 15 (txt $ view from a) <+> padLeft (Pad 2) (txt (view subject a))
    in (newMail a $ selected widget) <+> fillLine


listNewMailAttr :: AttrName
listNewMailAttr = L.listAttr <> "newmail"

-- | We currently have two modes on the main view we need to distinguish
-- keystrokes for. One is to obviously browse the mails which are shown as a
-- list, the other is to allow the user to easily change the list.
mainEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
mainEvent s e =
    case (s ^. asMailIndex ^. miMode) of
        BrowseMail ->
            handleEvent
                (s ^. asConfig ^. confIndexView ^. ivKeybindings)
                listEventDefault
                s
                e
        SearchMail ->
            handleEvent
                (s ^. asConfig ^. confIndexView ^. ivSearchKeybindings)
                searchInputEventDefault
                s
                e

-- | Handle key strokes on the list of mails.
-- Most keystrokes are delegated to the list of mails, while one particular
-- get's us out of the application and one is to signal that we want the focus
-- on the input field to change the notmuch search.
-- Note: moving around in the list clears any errors set during other events.
-- Maybe that will remove anything important for the user to see. Do we need
-- something like an error log?
listEventDefault :: AppState -> Event -> T.EventM Name (T.Next AppState)
listEventDefault s e =
    L.handleListEvent e (s ^. asMailIndex ^. miListOfMails) >>=
    \mi' ->
         M.continue $ s & asMailIndex . miListOfMails .~ mi' & asAppMode .~
         Main &
         asError .~
         Nothing


-- | Search search input is mostly straight forward, since every keystroke is
-- delegated to the widget, except applying the entered input or signaling to
-- move the focus back to the list of mails.
searchInputEventDefault :: AppState -> Event -> T.EventM Name (T.Next AppState)
searchInputEventDefault s ev =
    E.handleEditorEvent ev (s ^. asMailIndex ^. miSearchEditor) >>=
    \ed ->
         M.continue $ s & asMailIndex . miSearchEditor .~ ed & asAppMode .~
         Main
