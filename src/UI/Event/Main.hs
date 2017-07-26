-- | module for handling events on the main screen
module UI.Event.Main where

import qualified Brick.Main             as M
import qualified Brick.Types            as T
import qualified Brick.Widgets.Edit     as E
import qualified Brick.Widgets.List     as L
import           Control.Lens.Getter    ((^.))
import           Control.Lens.Lens      ((&))
import           Control.Lens.Setter    ((.~))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text              as T
import           Data.Text.Zipper       (currentLine)
import qualified Graphics.Vty           as V
import           Storage.Notmuch        (getMessages)
import           UI.Types

-- | We currently have two modes on the main view we need to distinguish
-- keystrokes for. One is to obviously browse the mails which are shown as a
-- list, the other is to allow the user to easily change the list.
mainEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
mainEvent s e =
    case (s^.mailIndex^.miMode) of
      BrowseMail -> handleListEvents s e
      SearchMail -> handleSearchInputEvents s e

-- | Handle key strokes on the list of mails.
-- Most keystrokes are delegated to the list of mails, while one particular
-- get's us out of the application and one is to signal that we want the focus
-- on the input field to change the notmuch search.
handleListEvents :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
handleListEvents s (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> M.halt s
        V.EvKey (V.KChar ':') [] -> M.continue $ mailIndex . miMode .~ SearchMail $ s
        V.EvKey V.KEnter [] -> M.continue $ appMode .~ ViewMail $ s
        ev ->
            L.handleListEvent ev (s ^. mailIndex ^. listOfMails) >>=
            \mi' ->
                 M.continue $ s & mailIndex . listOfMails .~ mi' & appMode .~
                 Main
handleListEvents s _ = M.continue s

-- | Search search input is mostly straight forward, since every keystroke is
-- delegated to the widget, except applying the entered input or signaling to
-- move the focus back to the list of mails.
handleSearchInputEvents :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
handleSearchInputEvents s (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> M.continue $ mailIndex . miMode .~ BrowseMail $ s
        V.EvKey V.KEnter [] -> do
            let searchterms =
                    currentLine $ s ^. mailIndex ^. searchEditor ^.
                    E.editContentsL
            vec <-
                liftIO $
                getMessages (s ^. notmuchDatabaseFp) (T.unpack searchterms)
            let listWidget = (L.list ListOfMails vec 1)
            M.continue $ s & mailIndex . listOfMails .~ listWidget & appMode .~
                Main
        ev ->
            E.handleEditorEvent ev (s ^. mailIndex ^. searchEditor) >>=
            \ed ->
                 M.continue $ s & mailIndex . searchEditor .~ ed & appMode .~
                 Main
handleSearchInputEvents s _ = M.continue s
