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
{-# LANGUAGE RankNTypes        #-}

module UI.GatherHeaders.Main (drawFrom, drawTo, drawSubject) where

import Control.Lens (view)

import Brick.Types (Widget)

import Types
import UI.Draw.Main (renderEditorWithLabel)
import UI.Views (focusedViewWidget)

drawFrom :: AppState -> Widget Name
drawFrom s = renderEditorWithLabel "From:" (ComposeFrom == focusedViewWidget s) (view (asCompose . cFrom) s)

drawTo :: AppState -> Widget Name
drawTo s = renderEditorWithLabel "To:" (ComposeTo == focusedViewWidget s) (view (asCompose . cTo) s)

drawSubject :: AppState -> Widget Name
drawSubject s = renderEditorWithLabel "Subject:" (ComposeSubject == focusedViewWidget s) (view (asCompose . cSubject) s)
