-- This file is part of purebred
-- Copyright (C) 2018 Róman Joost
--
-- purebred is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE OverloadedStrings #-}

module UI.Index.Main (
    renderListOfThreads
  , renderListOfMails) where

import Brick.Types (Padding(..), Widget)
import Brick.AttrMap (AttrName, attrName)
import Brick.Widgets.Core
  (hBox, hLimitPercent, padRight, padLeft, txt, vLimit, withAttr, (<+>))
import qualified Brick.Widgets.List as L
import Control.Lens.Getter (view)
import Data.Time.Clock
       (UTCTime(..), NominalDiffTime, nominalDay, diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Semigroup ((<>))
import Data.Text as T (Text, pack, unpack, unwords)

import Notmuch (getTag)

import UI.Draw.Main (fillLine)
import Storage.Notmuch (hasTag)
import Types
import Config.Main
       (listNewMailAttr, listNewMailSelectedAttr, mailTagAttr,
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
          [ padLeft (Pad 1) (txt $ formatDate (view mailDate a) (view asLocalTime s))
          , padLeft (Pad 1) (renderAuthors sel $ view mailFrom a)
          , padLeft (Pad 1) (renderTagsWidget (view mailTags a) (view nmNewTag settings))
          , txt (view mailSubject a)
          , fillLine
          ]
    in withAttr (getListAttr isNewMail sel) widget

listDrawThread :: AppState -> Bool -> NotmuchThread -> Widget Name
listDrawThread s sel a =
    let settings = view (asConfig . confNotmuch) s
        isNewMail = hasTag (view nmNewTag settings) a
        widget = hBox
          [ padLeft (Pad 1) (txt $ formatDate (view thDate a) (view asLocalTime s))
          , padLeft (Pad 1) (renderAuthors sel $ T.unwords $ view thAuthors a)
          , padLeft (Pad 1) (txt $ pack $ "(" <> show (view thReplies a) <> ")")
          , padLeft (Pad 1) (renderTagsWidget (view thTags a) (view nmNewTag settings))
          , txt (view thSubject a)
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

calendarYear :: NominalDiffTime
calendarYear = nominalDay * 365

formatDate :: UTCTime -> UTCTime -> Text
formatDate mail now =
  let format =
        if calendarYear < diffUTCTime now mail
          then "%b'%y"  -- Apr'07
          else "%d/%b"  -- 01/Apr
   in pack $ formatTime defaultTimeLocale format (utctDay mail)

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
        render tag = padRight (Pad 1) $ withAttr (toAttrName tag) $ txt (decodeLenient $ getTag tag)
    in vLimit 1 $ hBox $ render  <$> ts

toAttrName :: Tag -> AttrName
toAttrName = (mailTagAttr <>) . attrName . unpack . decodeLenient . getTag
