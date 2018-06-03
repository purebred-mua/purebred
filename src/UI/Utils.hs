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
       (safeUpdate, focusedViewWidget, focusedViewWidgets,
        focusedViewName, focusedView, titleize, Titleize)
       where
import qualified Data.Vector as Vector
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Control.Lens (view, at, _Just)
import Brick.Focus (focusGetCurrent)

import UI.Views (indexView)
import Types

safeUpdate :: Foldable t => Vector.Vector a -> t (Int, a) -> Vector.Vector a
safeUpdate v = (Vector.//) v  . filter ((\i -> i >= 0 && i < length v) . fst) . toList

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
  titleize m = pack $ show m

instance Titleize ViewName where
  titleize a = pack $ show a
