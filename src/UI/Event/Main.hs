module UI.Event.Main where

import qualified Brick.Main             as M
import qualified Brick.Types            as T
import qualified Brick.Widgets.Edit     as E
import qualified Brick.Widgets.List     as L
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text              as T
import           Data.Text.Zipper       (currentLine)
import qualified Graphics.Vty           as V
import           Lens.Micro             ((^.))
import           Storage.Notmuch        (getMessages)
import           UI.Types

mainEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
mainEvent s e =
    case (s^.mailIndex^.miMode) of
      BrowseMail -> handleListEvents s e
      SearchMail -> handleSearchInputEvents s e

handleListEvents :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
handleListEvents a@(AppState s db mi Main) (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> M.halt a
        V.EvKey (V.KChar ':') [] ->
            M.continue $
            (AppState
                 s
                 db
                 (MailIndex (mi ^. listOfMails) (mi ^. searchEditor) SearchMail)
                 Main)
        V.EvKey V.KEnter [] -> M.continue $ AppState s db mi ViewMail
        ev ->
            L.handleListEvent ev (mi ^. listOfMails) >>=
            \mi' ->
                 M.continue $
                 AppState
                     s
                     db
                     (MailIndex mi' (mi ^. searchEditor) (mi ^. miMode))
                     Main
handleListEvents s _ = M.continue s

handleSearchInputEvents :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
handleSearchInputEvents s (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] ->
            M.continue $
            AppState
                (s ^. notmuchRawsearch)
                (s ^. notmuchDatabaseFp)
                (MailIndex
                     (s ^. mailIndex ^. listOfMails)
                     (s ^. mailIndex ^. searchEditor)
                     BrowseMail)
                Main
        V.EvKey V.KEnter [] -> do
          let searchterms = currentLine $ s^.mailIndex^.searchEditor^.E.editContentsL
          vec <- liftIO $ getMessages (s^.notmuchDatabaseFp) (T.unpack searchterms)
          let mi = MailIndex (L.list ListOfMails vec 1) (s^.mailIndex^.searchEditor) BrowseMail
          M.continue $ AppState (s^.notmuchRawsearch) (s^.notmuchDatabaseFp) mi Main
        ev ->
            E.handleEditorEvent ev (s ^. mailIndex ^. searchEditor) >>=
            \ed ->
                 M.continue $
                 AppState
                     (s ^. notmuchRawsearch)
                     (s ^. notmuchDatabaseFp)
                     (MailIndex (s ^. mailIndex ^. listOfMails) ed SearchMail)
                     Main
handleSearchInputEvents s _ = M.continue s
