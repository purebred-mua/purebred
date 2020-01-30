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

-- | Utility functions for UI
module UI.Utils
  ( titleize
  , Titleize
  , toggledItems
  , selectedFiles
  ) where
import Data.List (union)

import Data.Text (Text, pack)
import Control.Lens
       (folded, traversed, filtered, toListOf, view, _2)
import qualified Brick.Widgets.List as L

import Types


toggledItems :: L.List Name (SelectableItem a) -> [SelectableItem a]
toggledItems = toListOf (L.listElementsL . folded . filtered fst)

-- | Toggle file list entries to be selected
--
selectedFiles :: L.List Name (SelectableItem FileSystemEntry) -> [FilePath]
selectedFiles l = let cur = case L.listSelectedElement l of
                        Just (_, (_, File fsname)) -> [(False, File fsname)]
                        _ -> []
                      toggled = view (_2 . fsEntryName) <$> toggledItems l
                  in toggled `union` toListOf (traversed . _2 . fsEntryName) cur

-- | Show a descriptive name
--
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
