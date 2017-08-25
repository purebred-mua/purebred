{-# LANGUAGE OverloadedStrings #-}
module UI.Index.Main where

import Brick.AttrMap (AttrName)
import Data.Monoid ((<>))
import qualified Brick.Main as M
import Brick.Types (Padding(..), Widget)
import qualified Brick.Types as T
import Brick.Widgets.Core
       (hLimit, padLeft, txt, vBox, vLimit, withAttr, (<+>))
import Data.Text (unwords)
import Prelude hiding (unwords)
import qualified Brick.Widgets.Edit        as E
import qualified Brick.Widgets.List        as L
import Control.Lens.Getter (view)
import Control.Lens.Lens ((&))
import Control.Lens.Setter (set)
import Graphics.Vty.Input.Events (Event)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Text (Text, pack)
import Storage.Mail (from, subject, mailTags, Mail, mailIsNew, mailDate)
import UI.Draw.Main (editorDrawContent)
import UI.Keybindings (handleEvent)
import UI.Status.Main (statusbar)
import Types

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
        widget = padLeft (Pad 1) $ (hLimit 15 (txt $ view from a)) <+>
                 (padLeft (Pad 1) $ (txt $ formatDate (view mailDate a))) <+>
                 padLeft (Pad 2) (txt (view subject a)) <+>
                 (padLeft Max $ renderMailTagsWidget a)
    in (newMail a $ selected widget)


formatDate :: UTCTime -> Text
formatDate t = pack $ formatTime defaultTimeLocale "%d/%b" (utctDay t)

listNewMailAttr :: AttrName
listNewMailAttr = L.listAttr <> "newmail"

mailAttr :: AttrName
mailAttr = "mail"

mailTagsAttr :: AttrName
mailTagsAttr = mailAttr <> "tags"

renderMailTagsWidget :: Mail -> Widget Name
renderMailTagsWidget m =
    let ts = view mailTags m
    in withAttr mailTagsAttr $ vLimit 1 $ txt $ unwords ts

-- | We currently have two modes on the main view we need to distinguish
-- keystrokes for. One is to obviously browse the mails which are shown as a
-- list, the other is to allow the user to easily change the list.
mainEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
mainEvent s e =
    case (view (asMailIndex . miMode) s) of
        BrowseMail ->
            handleEvent
                (view (asConfig . confIndexView . ivKeybindings) s)
                listEventDefault
                s
                e
        SearchMail ->
            handleEvent
                (view (asConfig . confIndexView . ivSearchKeybindings) s)
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
    L.handleListEvent e (view (asMailIndex . miListOfMails) s) >>=
    \mi' ->
         M.continue $
         set (asMailIndex . miListOfMails) mi' s & set asAppMode Main &
         set asError Nothing


-- | Search search input is mostly straight forward, since every keystroke is
-- delegated to the widget, except applying the entered input or signaling to
-- move the focus back to the list of mails.
searchInputEventDefault :: AppState -> Event -> T.EventM Name (T.Next AppState)
searchInputEventDefault s ev =
    E.handleEditorEvent ev (view (asMailIndex . miSearchEditor) s) >>=
    \ed ->
         M.continue $
         set (asMailIndex . miSearchEditor) ed s & set asAppMode Main
