-- This file is part of purebred
-- Copyright (C) 2022  Fraser Tweedale
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

module Purebred.Types.Presentation
  ( BodyPresentation(..)
  , emptyBodyPresentation
  , MatchInfo(..)
  , MatchAtLine(..)
  ) where

import qualified Data.Text as T
import Numeric.Natural

import Brick (Widget, emptyWidget)

import Purebred.Types.UI (Name)

-- | Information about whether there is a search, and if so,
-- how many matches were found.
--
data MatchInfo
  = NoSearch
  | MatchCount Natural

-- | Data about the line number in the rendered widget
-- a match appears.
--
data MatchAtLine
  = NoMatch
  | MatchAtLine Natural

data BodyPresentation = BodyPresentation
  { widget :: Natural -> (MatchAtLine, Widget Name)
    -- ^ Yield the widget, highlighting the substring match of the
    -- given index (if any).

  , substringSearch :: T.Text -> (MatchInfo, BodyPresentation)
    -- ^ Perform the substring search and yield a new presentation.
    -- Empty search term means to reset the search.

  , toTextLines :: [T.Text]
    -- ^ Yield the body as lines of text, suitable for
    -- use as a reply (without blockquoting).
  }

emptyBodyPresentation :: BodyPresentation
emptyBodyPresentation =
  let
    this = BodyPresentation
      { widget = const (NoMatch, emptyWidget)
      , substringSearch = \term ->
          (if T.null term then NoSearch else MatchCount 0, this)
      , toTextLines = []
      }
  in
    this
