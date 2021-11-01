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
  ( StatefulEditor
  , statefulEditor
  , editEditorL
  , saveEditorState
  , revertEditorState
  ) where

import Control.Lens (Lens', lens)
import Brick.Widgets.Edit (Editor)

data StatefulEditor t n =
  StatefulEditor
    { _editState :: Editor t n
    , _editEditor :: Editor t n
    }

statefulEditor :: Editor t n -> StatefulEditor t n
statefulEditor ed = StatefulEditor ed ed

-- | Access to the underlying Brick Edit widget. 
editEditorL :: Lens' (StatefulEditor t n) (Editor t n)
editEditorL = lens _editEditor (\e v -> e { _editEditor = v})

-- | Save the editor state to potentially revert the editor back to it
-- later.
saveEditorState :: StatefulEditor t n -> StatefulEditor t n
saveEditorState (StatefulEditor _saved cur) = StatefulEditor cur cur

-- | Revert the editor back to it's previously saved contents.
revertEditorState :: StatefulEditor t n -> StatefulEditor t n
revertEditorState (StatefulEditor saved _cur)  = StatefulEditor saved saved
