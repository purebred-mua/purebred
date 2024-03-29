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

module Purebred.UI.Index.Main (
    renderListOfThreads
  , renderListOfMails) where

import Control.Lens.Getter (view)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Time.Clock
       (UTCTime(..), NominalDiffTime, nominalDay, diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Text as T

import Brick
  ( AttrName, Location(..), Padding(..), Widget
  , (<+>), attrName, emptyWidget, hBox, hLimitPercent
  , padLeft, putCursor, txt, vLimit, withAttr
  )
import qualified Brick.Widgets.List as L

import Notmuch (getTag)

import Purebred.Storage.Tags (hasTag, ManageTags)
import Purebred.Types
import Purebred.UI.Attr
  ( listAttr, listStateNewmailAttr, listStateSelectedAttr
  , listStateToggledAttr, mailAuthorsAttr, mailTagAttr )
import Purebred.UI.Draw.Main (fillLine)
import Purebred.UI.Views (focusedViewWidget)

renderListOfThreads :: AppState -> Widget Name
renderListOfThreads s = L.renderList (listDrawThread s (ListOfThreads == focusedViewWidget s)) True $ view (asThreadsView . miListOfThreads) s

renderListOfMails :: AppState -> Widget Name
renderListOfMails s = L.renderList (listDrawMail s) True $ view (asThreadsView . miListOfMails) s

notmuchConfig :: AppState -> NotmuchSettings
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
          [ txt $ formatDate (view mailDate a) (view asLocalTime s)
          , padLeft (Pad 1)
              $ hLimitPercent 25
              $ renderAuthors (authorsAttr a s sel toggled) (view mailFrom a)
          , renderTagsWidget'
              (tagsAttr a s sel toggled) (view mailTags a)
              (view nmNewTag (notmuchConfig s)) tagReplacements
          , padLeft (Pad 1) $ txt (view mailSubject a)
          , fillLine
          ]
        tagReplacements = view (asConfig . confIndexView . ivTagReplacementMap) s
    in withAttr (renderListAttr a s sel toggled) widget

listDrawThread :: AppState -> Bool -> Bool -> Toggleable NotmuchThread -> Widget Name
listDrawThread s foc sel (toggled, a) =
    let widget = hBox
          [ txt $ formatDate (view thDate a) (view asLocalTime s)
          , padLeft (Pad 1)
              $ hLimitPercent 25
              $ renderAuthors (authorsAttr a s sel toggled) (T.unwords $ view thAuthors a)
                <+> padLeft (Pad 1) (txt $ T.pack $ "(" <> show (view thReplies a) <> ")")
          , renderTagsWidget'
              (tagsAttr a s sel toggled) (view thTags a)
              (view nmNewTag (notmuchConfig s)) tagReplacements
          , padLeft (Pad 1) $ txt (view thSubject a)
          , fillLine
          ]
        tagReplacements = view (asConfig . confIndexView . ivTagReplacementMap) s
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

formatDate :: UTCTime -> UTCTime -> T.Text
formatDate mail now =
  let format =
        if calendarYear < diffUTCTime now mail
          then "%b'%y"  -- Apr'07
          else "%d/%b"  -- 01/Apr
   in T.pack $ formatTime defaultTimeLocale format (utctDay mail)

renderAuthors :: AttrName -> T.Text -> Widget Name
renderAuthors attr authors =
    withAttr attr $ txt authors <+> fillLine

renderTagsWidget' :: AttrName -> [Tag] -> Tag -> M.Map T.Text T.Text -> Widget Name
renderTagsWidget' baseattr tgs ignored replacements =
    let
      discrim tag =
        let decoded = decodeLenient (getTag tag)
        in maybe (Right decoded) Left (M.lookup decoded replacements)
      lr = discrim <$> filter (/= ignored) tgs
      replaced = case mapMaybe (either Just (const Nothing)) lr of
        [] -> emptyWidget
        l -> pad $ txt $ T.concat l
      ordinaries = mapMaybe (either (const Nothing) Just) lr
      pad = padLeft (Pad 1)
      renderOrdinary tag = pad $ withAttr (toAttrName baseattr tag) $ txt tag
    in vLimit 1 $ hBox $ replaced : fmap renderOrdinary ordinaries

toAttrName :: AttrName -> T.Text -> AttrName
toAttrName baseattr = (baseattr <>) . attrName . T.unpack
