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

{- |

Types and functions for preparing messages for display.

-}
module Purebred.Types.Display
  ( parseMailbody
  ) where

import Text.Wrap (defaultWrapSettings, wrapTextToLines)
import qualified Data.Text as T

import Purebred.Types


parseMailbody ::
     Int -- ^ text width
  -> Source
  -> T.Text
  -> MailBody
parseMailbody tw s =
  MailBody s . fmap (Line []) . wrapTextToLines defaultWrapSettings tw
