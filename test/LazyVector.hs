-- This file is part of purebred
-- Copyright (C) 2018 Fraser Tweedale
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

module LazyVector (tests) where

import Prelude hiding (splitAt)

import Data.Foldable (toList)

import Test.Tasty
import Test.Tasty.QuickCheck

import Purebred.Types.LazyVector

tests :: TestTree
tests = testGroup "LazyVector tests"
  [ testProperty "splitAt length" prop_splitAtLength
  , testProperty "splitAt append" prop_splitAtAppend
  , testProperty "V is lazy" prop_lazy
  , testProperty "fromList" prop_fromList
  ]


-- This property checks that splitAt splits at the right place.
--
prop_splitAtLength :: [()] -> Int -> Bool
prop_splitAtLength xs i =
  let
    l = fromList 10 xs
    len = length l
    (h, t) = splitAt i l
  in
    length h + length t == len
    && length h == max 0 (min len i)

-- This property checks that splitAt keeps all elements
-- in the correct order.
--
-- Incidentally, it also checks that (==) ignores chunks and
-- only cares about the elements.
--
prop_splitAtAppend :: [Int] -> Int -> Bool
prop_splitAtAppend xs i =
  let l = fromList 10 xs
  in uncurry (<>) (splitAt i l) == l

-- This property check that V really is lazy
--
prop_lazy :: Bool
prop_lazy =
  let
    xs = 1:2:3:4:undefined
    l = fromList 4 xs :: V Int
    (h,_) = splitAt 4 l
  in
    h == fromList 4 (take 4 xs)
    && take 4 (toList l) == take 4 xs

-- Check that fromList keeps all elements in the correct order.
--
prop_fromList :: Int -> [Int] -> Bool
prop_fromList chunkSize xs = toList (fromList chunkSize xs) == xs
