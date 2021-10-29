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
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Index.Main (
    renderListOfThreads
  , renderListOfMails) where

import Brick.Types (Location(..), Padding(..), Widget)
import Brick.AttrMap (AttrName, attrName)
import Brick.Widgets.Core
  (hBox, hLimitPercent, padRight, padLeft, putCursor, txt, vLimit, withAttr, (<+>))
import qualified Brick.Widgets.List as L
import Control.Lens.Getter (view)
import Data.Time.Clock
       (UTCTime(..), NominalDiffTime, nominalDay, diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Text as T (Text, pack, unpack, unwords)

import Notmuch (getTag)

import UI.Draw.Main (fillLine)
import UI.Views (focusedViewWidget)
import Purebred.Storage.Notmuch (hasTag, ManageTags)
import Types
import Purebred.Config
  (listAttr, listStateNewmailAttr, listStateSelectedAttr,
  listStateToggledAttr, mailAuthorsAttr, mailTagAttr)

renderListOfThreads :: AppState -> Widget Name
renderListOfThreads s = L.renderList (listDrawThread s (ListOfThreads == focusedViewWidget s)) True $ view (asThreadsView . miListOfThreads) s

renderListOfMails :: AppState -> Widget Name
renderListOfMails s = L.renderList (listDrawMail s) True $ view (asThreadsView . miListOfMails) s

notmuchConfig :: AppState -> NotmuchSettings FilePath
notmuchConfig = view (asConfig . confNotmuch)

isNewMail :: ManageTags a => a -> AppState -> Bool
isNewMail a s = hasTag (view nmNewTag (notmuchConfig s)) a

renderListAttr, authorsAttr, tagsAttr ::
     ManageTags a
  => a
  -> AppState
  -> Bool -- ^ selected
  -> Bool -- ^ Toggled
  -> AttrName
renderListAttr a s = makeListStateAttr listAttr (isNewMail a s)
authorsAttr a s = makeListStateAttr mailAuthorsAttr (isNewMail a s)
tagsAttr a s = makeListStateAttr mailTagAttr (isNewMail a s)


listDrawMail :: AppState -> Bool -> Toggleable NotmuchMail -> Widget Name
listDrawMail s sel (toggled, a) =
    let widget = hBox
          -- NOTE: I believe that inserting a `str " "` is more efficient than
          -- `padLeft/Right (Pad 1)`.  This hypothesis should be tested.
          [ padLeft (Pad 1) (txt $ formatDate (view mailDate a) (view asLocalTime s))
          , padLeft (Pad 1) (renderAuthors (authorsAttr a s sel toggled) $ view mailFrom a)
          , padLeft (Pad 1) (renderTagsWidget' (tagsAttr a s sel toggled) (view mailTags a) (view nmNewTag (notmuchConfig s)))
          , txt (view mailSubject a)
          , fillLine
          ]
    in withAttr (renderListAttr a s sel toggled) widget

listDrawThread :: AppState -> Bool -> Bool -> Toggleable NotmuchThread -> Widget Name
listDrawThread s foc sel (toggled, a) =
    let widget = hBox
          [ padLeft (Pad 1) (txt $ formatDate (view thDate a) (view asLocalTime s))
          , padLeft (Pad 1) (renderAuthors (authorsAttr a s sel toggled) $ T.unwords $ view thAuthors a)
          , padLeft (Pad 1) (txt $ pack $ "(" <> show (view thReplies a) <> ")")
          , padLeft (Pad 1) (renderTagsWidget' (tagsAttr a s sel toggled) (view thTags a) (view nmNewTag (notmuchConfig s)))
          , txt (view thSubject a)
          , fillLine
          ]
    in withAttr (renderListAttr a s sel toggled)
       . (if sel && foc then putCursor ListOfMails (Location (0, 0)) else id) $
       widget

-- | Creates a widget attribute based on list item states: whether the
-- list item is new, currently selected (a.k.a focused) or
-- toggled. Outcome is an Attribute which has each state encoded in
-- the attribute if the state is true. For example:
--
-- @
-- $ let attr = makeListStateAttr listAttr True False True
-- $ show attr
-- AttrName ["list", "newmail", "toggled"]
--
makeListStateAttr ::
     AttrName
  -> Bool -- ^ new?
  -> Bool -- ^ selected?
  -> Bool -- ^ toggled?
  -> AttrName
makeListStateAttr baseAttr isNew isSelected isToggled =
  let newAttr = if isNew then listStateNewmailAttr else mempty
      selectedAttr = if isSelected then listStateSelectedAttr else mempty
      toggledAttr = if isToggled then listStateToggledAttr else mempty
  in baseAttr <> selectedAttr <> toggledAttr <> newAttr

calendarYear :: NominalDiffTime
calendarYear = nominalDay * 365

formatDate :: UTCTime -> UTCTime -> Text
formatDate mail now =
  let format =
        if calendarYear < diffUTCTime now mail
          then "%b'%y"  -- Apr'07
          else "%d/%b"  -- 01/Apr
   in pack $ formatTime defaultTimeLocale format (utctDay mail)

renderAuthors :: AttrName -> Text -> Widget Name
renderAuthors attr authors =
    withAttr attr $ hLimitPercent 20 (txt authors <+> fillLine)

renderTagsWidget' :: AttrName -> [Tag] -> Tag -> Widget Name
renderTagsWidget' baseattr tgs ignored =
    let ts = filter (/= ignored) tgs
        render tag = padRight (Pad 1) $ withAttr (toAttrName baseattr tag) $ txt (decodeLenient $ getTag tag)
    in vLimit 1 $ hBox $ render  <$> ts

toAttrName :: AttrName -> Tag -> AttrName
toAttrName baseattr = (baseattr <>) . attrName . unpack . decodeLenient . getTag
