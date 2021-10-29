-- This file is part of purebred
-- Copyright (C) 2020 Róman Joost
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

-- | This module provides a text editor widget like
-- Brick.Widgets.Edit. However it can be used to store it's state in
-- order to roll it back. This is useful if newly changed editor state is
-- to be canceled and to be rolled back to the previous state.
module Purebred.UI.Widgets
  ( StatefulEditor(..)
  , editStateL
  , editEditorL
  , saveEditorState
  , revertEditorState
  ) where

import Control.Lens (Lens', lens, view, set, to, over)
import Data.Text.Zipper (currentLine, insertMany, clearZipper)
import Brick.Widgets.Edit (Editor(..), editContentsL)

data StatefulEditor t n =
  StatefulEditor
    { _editState :: t
    , _editEditor :: Editor t n
    }


-- | Access to the editors state.
-- Note: Do not rely on accessing the editor contents using this
-- lens. Always access the editor contents directly by accessing the
-- editor itself using 'editEditorL'.
editStateL :: Lens' (StatefulEditor t n) t
editStateL = lens _editState (\e v -> e { _editState = v})

-- | Access to the underlying Brick Edit widget. 
editEditorL :: Lens' (StatefulEditor t n) (Editor t n)
editEditorL = lens _editEditor (\e v -> e { _editEditor = v})

-- | Save the editor state to potentially revert the editor back to it
-- later.
saveEditorState :: Monoid t => StatefulEditor t n -> StatefulEditor t n
saveEditorState editor =
  let contents = view (editEditorL . editContentsL . to currentLine) editor
   in set editStateL contents editor

-- | Revert the editor back to it's previously saved contents.
revertEditorState :: Monoid t => StatefulEditor t n -> StatefulEditor t n
revertEditorState editor =
  let saved = view editStateL editor
   in over (editEditorL . editContentsL) (insertMany saved . clearZipper) editor
