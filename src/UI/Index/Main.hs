{-# LANGUAGE OverloadedStrings #-}
module UI.Index.Main where

import qualified Brick.Main                as M
import           Brick.Types               (Padding (..), Widget)
import qualified Brick.Types               as T
import           Brick.Widgets.Core        (hLimit, padLeft, str, vBox, vLimit,
                                            withAttr, (<+>))

import qualified Brick.Widgets.Edit        as E
import qualified Brick.Widgets.List        as L
import           Config.Types              (confIndexView, ivKeybindings)
import           Control.Lens.Getter       ((^.))
import           Control.Lens.Lens         ((&))
import           Control.Lens.Setter       ((.~))
import           Graphics.Vty.Input.Events (Event)
import           Storage.Mail
import           UI.Draw.Main              (editorDrawContent, fillLine)
import           UI.Keybindings            (handleEvent, indexsearchKeybindings)
import           UI.Status.Main            (statusbar)
import           UI.Types

drawMain :: AppState -> [Widget Name]
drawMain s = [ui]
  where
    editorFocus = case (s^.asMailIndex^.miMode) of
      BrowseMail -> False
      SearchMail -> True
    inputBox = E.renderEditor editorDrawContent editorFocus (s ^. asMailIndex ^. miSearchEditor)
    ui = vBox [renderMailList s, statusbar s, vLimit 1 inputBox]

renderMailList :: AppState -> Widget Name
renderMailList s = L.renderList listDrawElement False (s ^. asMailIndex ^. miListOfMails)

listDrawElement :: Bool -> Mail -> Widget Name
listDrawElement sel a =
    let selStr w =
            if sel
                then withAttr L.listSelectedAttr w <+> fillLine
                else w
    in (selStr $
        padLeft (Pad 1) $
        hLimit 15 (str $ a ^. from) <+> padLeft (Pad 2) (str (a ^. subject)))


-- | We currently have two modes on the main view we need to distinguish
-- keystrokes for. One is to obviously browse the mails which are shown as a
-- list, the other is to allow the user to easily change the list.
mainEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
mainEvent s e =
    case (s^.asMailIndex^.miMode) of
      BrowseMail -> handleEvent (s^.asConfig^.confIndexView^.ivKeybindings) listEventDefault s e
      SearchMail -> handleEvent indexsearchKeybindings searchInputEventDefault s e

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
