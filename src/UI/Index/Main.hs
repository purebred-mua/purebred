{-# LANGUAGE OverloadedStrings #-}

module UI.Index.Main where

import qualified Brick.Main as M
import Brick.Types (Padding(..), Widget)
import qualified Brick.Types as T
import Brick.AttrMap (AttrName)
import Brick.Widgets.Core
       (hLimit, padLeft, txt, vBox, vLimit, withAttr, (<+>))
import Prelude hiding (unwords)
import qualified Brick.Widgets.Edit        as E
import qualified Brick.Widgets.List        as L
import Control.Lens.Getter (view)
import Control.Lens.Lens ((&))
import Control.Lens.Setter (set)
import Graphics.Vty.Input.Events (Event)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Text (Text, pack, unwords)
import UI.Draw.Main (editorDrawContent)
import UI.Keybindings (handleEvent)
import UI.Status.Main (statusbar)
import Storage.Notmuch (mailIsNew)
import Types
import Config.Main
       (listNewMailAttr, listNewMailSelectedAttr, mailTagsAttr,
        listSelectedAttr, listAttr)

drawMain :: AppState -> [Widget Name]
drawMain s = [ui]
  where
    editorFocus = view asAppMode s `elem` [SearchMail, ManageTags]
    inputBox = E.renderEditor editorDrawContent editorFocus (view (asMailIndex . miSearchEditor) s)
    ui = vBox [renderMailList s, statusbar s, vLimit 1 inputBox]

renderMailList :: AppState -> Widget Name
renderMailList s = let listFocus = view asAppMode s == BrowseMail
                   in L.renderList (listDrawElement s) listFocus (view (asMailIndex . miListOfMails) s)

listDrawElement :: AppState -> Bool -> NotmuchMail -> Widget Name
listDrawElement s sel a =
    let settings = view (asConfig . confNotmuch) s
        isNewMail = mailIsNew (view nmNewTag settings) a
        widget = padLeft (Pad 1) $ hLimit 15 (txt $ view mailFrom a) <+>
                 padLeft (Pad 1) (txt $ formatDate (view mailDate a)) <+>
                 padLeft (Pad 2) (txt (view mailSubject a)) <+>
                 padLeft Max (renderMailTagsWidget a (view nmNewTag settings))
    in withAttr (getListAttr isNewMail sel) widget

getListAttr :: Bool  -- ^ new?
            -> Bool  -- ^ selected?
            -> AttrName
getListAttr True True = listNewMailSelectedAttr  -- new and selected
getListAttr True False = listNewMailAttr  -- new and not selected
getListAttr False True = listSelectedAttr  -- not new but selected
getListAttr False False = listAttr  -- not selected and not new

formatDate :: UTCTime -> Text
formatDate t = pack $ formatTime defaultTimeLocale "%d/%b" (utctDay t)

renderMailTagsWidget :: NotmuchMail -> Text -> Widget Name
renderMailTagsWidget m ignored =
    let ts = filter (/= ignored) $ view mailTags m
    in withAttr mailTagsAttr $ vLimit 1 $ txt $ unwords ts

-- | We currently have two modes on the main view we need to distinguish
-- keystrokes for. One is to obviously browse the mails which are shown as a
-- list, the other is to allow the user to easily change the list.
mainEvent :: AppState -> Event -> T.EventM Name (T.Next AppState)
mainEvent s =
    case view asAppMode s of
        BrowseMail ->
            handleEvent
                (view (asConfig . confIndexView . ivKeybindings) s)
                listEventDefault
                s
        ManageTags ->
            handleEvent
                (view (asConfig . confIndexView . ivManageTagsKeybindings) s)
                searchInputEventDefault
                s
        _ ->
            handleEvent
                (view (asConfig . confIndexView . ivSearchKeybindings) s)
                searchInputEventDefault
                s

-- | Handle key strokes on the list of mails.
-- Most keystrokes are delegated to the list of mails, while one particular
-- get's us out of the application and one is to signal that we want the focus
-- on the input field to change the notmuch search.
-- Note: moving around in the list clears any errors set during other events.
-- Maybe that will remove anything important for the user to see. Do we need
-- something like an error log?
listEventDefault :: AppState -> Event -> T.EventM Name (T.Next AppState)
listEventDefault s e = M.continue =<< T.handleEventLensed s (asMailIndex . miListOfMails) L.handleListEvent e


-- | Search search input is mostly straight forward, since every keystroke is
-- delegated to the widget, except applying the entered input or signaling to
-- move the focus back to the list of mails.
searchInputEventDefault :: AppState -> Event -> T.EventM Name (T.Next AppState)
searchInputEventDefault s ev =
    E.handleEditorEvent ev (view (asMailIndex . miSearchEditor) s) >>=
    \ed ->
         M.continue $
         set (asMailIndex . miSearchEditor) ed s
