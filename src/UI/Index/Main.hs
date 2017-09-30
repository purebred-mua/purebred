{-# LANGUAGE OverloadedStrings #-}

module UI.Index.Main where

import Brick.Types (Padding(..), Widget)
import Brick.AttrMap (AttrName)
import Brick.Widgets.Core
       (hLimit, padLeft, txt, vBox, vLimit, withAttr, (<+>))
import Prelude hiding (unwords)
import qualified Brick.Widgets.Edit        as E
import qualified Brick.Widgets.List        as L
import Control.Lens.Getter (view)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Text (Text, pack, unwords)
import UI.Draw.Main (editorDrawContent)
import UI.Status.Main (statusbar)
import Storage.Notmuch (mailIsNew)
import Types
import Config.Main
       (listNewMailAttr, listNewMailSelectedAttr, mailTagsAttr,
        listSelectedAttr, listAttr)

drawMain :: AppState -> [Widget Name]
drawMain s = [ui]
  where
    editorFocus = view asAppMode s == SearchMail
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

