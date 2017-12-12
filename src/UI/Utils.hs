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
module UI.Utils where
import qualified Data.Vector as Vector
import Data.Foldable (toList)

safeUpdate :: Foldable t => Vector.Vector a -> t (Int, a) -> Vector.Vector a
safeUpdate v = (Vector.//) v  . filter ((\i -> i >= 0 && i < length v) . fst) . toList
