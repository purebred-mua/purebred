{-# LANGUAGE OverloadedStrings #-}

module UI.Index.Main (
    renderListOfThreads
  , renderListOfMails) where

import Brick.Types (Padding(..), Widget)
import Brick.AttrMap (AttrName)
import Brick.Widgets.Core
  (hBox, hLimitPercent, padLeft, txt, vLimit, withAttr, (<+>))
import qualified Brick.Widgets.List as L
import Control.Lens.Getter (view)
import qualified Data.ByteString as B
import Data.Time.Clock (UTCTime(..))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Semigroup ((<>))
import Data.Text as T (Text, pack, unwords)

import Notmuch (getTag)

import UI.Draw.Main (fillLine)
import Storage.Notmuch (hasTag)
import Types
import Config.Main
       (listNewMailAttr, listNewMailSelectedAttr, mailTagsAttr,
        listSelectedAttr, listAttr, mailAuthorsAttr,
        mailSelectedAuthorsAttr)

renderListOfThreads :: AppState -> Widget Name
renderListOfThreads s = L.renderList (listDrawThread s) True $ view (asMailIndex . miListOfThreads) s

renderListOfMails :: AppState -> Widget Name
renderListOfMails s = L.renderList (listDrawMail s) True $ view (asMailIndex . miListOfMails) s

listDrawMail :: AppState -> Bool -> NotmuchMail -> Widget Name
listDrawMail s sel a =
    let settings = view (asConfig . confNotmuch) s
        isNewMail = hasTag (view nmNewTag settings) a
        widget = hBox
          -- NOTE: I believe that inserting a `str " "` is more efficient than
          -- `padLeft/Right (Pad 1)`.  This hypothesis should be tested.
          [ padLeft (Pad 1) (txt $ formatDate (view mailDate a))
          , padLeft (Pad 1) (renderAuthors sel $ view mailFrom a)
          , padLeft (Pad 1) (renderTagsWidget (view mailTags a) (view nmNewTag settings))
          , padLeft (Pad 1) (txt (view mailSubject a))
          , fillLine
          ]
    in withAttr (getListAttr isNewMail sel) widget

listDrawThread :: AppState -> Bool -> NotmuchThread -> Widget Name
listDrawThread s sel a =
    let settings = view (asConfig . confNotmuch) s
        isNewMail = hasTag (view nmNewTag settings) a
        widget = hBox
          [ padLeft (Pad 1) (txt $ formatDate (view thDate a))
          , padLeft (Pad 1) (renderAuthors sel $ T.unwords $ view thAuthors a)
          , padLeft (Pad 1) (txt $ pack $ "(" <> show (view thReplies a) <> ")")
          , padLeft (Pad 1) (renderTagsWidget (view thTags a) (view nmNewTag settings))
          , padLeft (Pad 1) (txt (view thSubject a))
          , fillLine
          ]
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

renderAuthors :: Bool -> Text -> Widget Name
renderAuthors isSelected authors =
    let attribute =
            if isSelected
                then mailSelectedAuthorsAttr
                else mailAuthorsAttr
    in withAttr attribute $ hLimitPercent 20 (txt authors <+> fillLine)

renderTagsWidget :: [Tag] -> Tag -> Widget Name
renderTagsWidget tgs ignored =
    let ts = filter (/= ignored) tgs
    in withAttr mailTagsAttr $ vLimit 1 $ txt $ decodeLenient $ B.intercalate " " $ fmap getTag ts
