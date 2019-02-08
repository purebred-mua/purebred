{-# LANGUAGE RankNTypes #-}
-- This file is part of purebred
-- Copyright (C) 2017 Fraser Tweedale and Róman Joost
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
--
{-# LANGUAGE OverloadedStrings #-}
module UI.Utils
  ( focusedViewWidget
  , focusedViewWidgets
  , focusedViewName
  , focusedView
  , titleize
  , Titleize
  , toggledItems
  , selectedFiles
  , takeFileName
  ) where
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.List (union)
import qualified System.FilePath as FP (takeFileName)
import Control.Lens
       (folded, traversed, filtered, toListOf, view, at, _Just, _2)
import Brick.Focus (focusGetCurrent)
import qualified Brick.Widgets.List as L

import UI.Views (indexView)


import Types

focusedViewWidget :: AppState -> Name -> Name
focusedViewWidget s defaultWidget =
    let ring = view vFocus (focusedView s)
    in fromMaybe defaultWidget $ focusGetCurrent ring

focusedViewWidgets :: AppState -> [Name]
focusedViewWidgets s =
    let defaultV = view (asConfig . confDefaultView) s
        focused = fromMaybe defaultV $ focusGetCurrent $ view (asViews . vsFocusedView) s
    in view (asViews . vsViews . at focused . _Just . vWidgets) s

focusedViewName :: AppState -> ViewName
focusedViewName s =
    let defaultV = view (asConfig . confDefaultView) s
    in fromMaybe defaultV $ focusGetCurrent $ view (asViews . vsFocusedView) s

focusedView :: AppState -> View
focusedView s = let focused = view (asViews . vsViews . at (focusedViewName s)) s
                in fromMaybe indexView focused

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
  titleize ListOfAttachments = "Attachments"
  titleize ManageFileBrowserSearchPath = "Filepath for Directory Listing"
  titleize m = pack $ show m

instance Titleize ViewName where
  titleize a = pack $ show a
