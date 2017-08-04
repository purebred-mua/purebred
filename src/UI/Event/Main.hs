-- | module for handling events on the main screen
module UI.Event.Main where

import qualified Brick.Main                as M
import qualified Brick.Types               as T
import qualified Brick.Widgets.Edit        as E
import qualified Brick.Widgets.List        as L
import           Control.Lens.Getter       ((^.))
import           Control.Lens.Lens         ((&))
import           Control.Lens.Setter       ((.~))
import           Graphics.Vty.Input.Events (Event)
import           UI.Keybindings            (handleEvent, indexKeybindings,
                                            indexsearchKeybindings)
import           UI.Types

-- | We currently have two modes on the main view we need to distinguish
-- keystrokes for. One is to obviously browse the mails which are shown as a
-- list, the other is to allow the user to easily change the list.
mainEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
mainEvent s e =
    case (s^.asMailIndex^.miMode) of
      BrowseMail -> handleEvent indexKeybindings listEventDefault s e
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
