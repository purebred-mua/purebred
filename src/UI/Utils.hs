-- This file is part of purebred
-- Copyright (C) 2017-2019 Fraser Tweedale and Róman Joost
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

module UI.Utils
  ( titleize
  , Titleize
  , toggledItems
  , selectedFiles
  , takeFileName
  , sanitise
  ) where

import Data.Char (chr, isControl, ord)
import Data.List (union)

import qualified Data.Text as T
import Data.Text (Text, pack, unpack)
import qualified System.FilePath as FP (takeFileName)
import Control.Lens
       (folded, traversed, filtered, toListOf, view, _2)
import qualified Brick.Widgets.List as L

import Types


toggledItems :: L.List Name (Bool, a) -> [(Bool, a)]
toggledItems = toListOf (L.listElementsL . folded . filtered fst)

selectedFiles :: L.List Name (Bool, FileSystemEntry) -> [FilePath]
selectedFiles l = let cur = case L.listSelectedElement l of
                        Just (_, (_, File fsname)) -> [(False, File fsname)]
                        _ -> []
                      toggled = view (_2 . fsEntryName) <$> toggledItems l
                  in toggled `union` toListOf (traversed . _2 . fsEntryName) cur

takeFileName :: Text -> Text
takeFileName = pack . FP.takeFileName . unpack

class Titleize a where
  titleize :: a -> Text

instance Titleize Name where
  titleize ListOfMails = "List of Mails"
  titleize ListOfThreads = "List of Threads"
  titleize ManageThreadTagsEditor = "Edit Labels of Threads"
  titleize ManageMailTagsEditor = "Edit Labels of Mails"
  titleize SearchThreadsEditor = "Search Editor"
  titleize ScrollingMailView = "Mail Viewer"
  titleize ComposeTo = "Editor to Compose a new Mail"
  titleize ComposeFrom = "Editor to Compose a new Mail"
  titleize ComposeSubject = "Editor to Compose a new Mail"
  titleize ListOfFiles = "Directory Listing"
  titleize ComposeListOfAttachments = "Attachments"
  titleize ManageFileBrowserSearchPath = "Filepath for Directory Listing"
  titleize MailAttachmentOpenWithEditor = "Open With Editor"
  titleize MailAttachmentPipeToEditor = "Pipe to Editor"
  titleize m = pack $ show m

instance Titleize ViewName where
  titleize a = pack $ show a

-- | Convert or strip control characters from input.
--
-- * Tab (HT) is replaced with 8 spaces.
-- * Other C0 codes (except CR and LF) and DEL are replaced with
--   <https://en.wikipedia.org/wiki/Control_Pictures Control Pictures>
-- * C1 and all other control characters are replaced with
--   REPLACEMENT CHARACTER U+FFFD
--
sanitise :: T.Text -> T.Text
sanitise = T.map substControl . T.replace "\t" "        "
  where
  substControl c
    | c == '\n' || c == '\r' = c  -- CR and LF are OK
    | c <= '\x1f' = chr (0x2400 + ord c)
    | c == '\DEL' = '\x2421'
    | isControl c = '\xfffd' -- REPLACEMENT CHARACTER
    | otherwise = c
