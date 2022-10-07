-- This file is part of purebred
-- Copyright (C) 2018-2020 Fraser Tweedale
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

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{- |

Lazy vectors.

-}
module Purebred.Types.LazyVector
  ( V
  , fromList
  , fromChunks
  , splitAt
  ) where

import Data.Bool (otherwise)
import Data.Eq (Eq(..))
import Data.Foldable (Foldable, length)
import Data.Function ((.))
import Data.Functor (Functor)
import Data.Int (Int)
import qualified Data.List as L
import Data.Monoid (Monoid(..))
import Data.Ord (Ord(..))
import Data.Semigroup (Semigroup(..))
import Prelude ((+), (-), uncurry)
import Text.Show (Show)

import qualified Brick.Widgets.List as Brick
import Control.Lens
import qualified Data.Vector as V

-- | A lazy vector.  Linked list of chunks.
newtype V a = V [V.Vector a]
  deriving (Functor, Foldable, Traversable, Show)

instance Semigroup (V a) where
  V l <> V r = V (l <> r)

instance Monoid (V a) where
  mempty = V []

-- | /Θ(n)/ Concatenates chunks and checks equality.
instance Eq a => Eq (V a) where
  V l == V r = V.concat l == V.concat r

-- | /Θ(n)/ Concatenates chunks and compares.
instance Ord a => Ord (V a) where
  V l `compare` V r = V.concat l `compare` V.concat r

instance Brick.Splittable V where
  splitAt = splitAt


-- | Lazily construct a V, slurping one chunk at a time
-- from the input list, with chunk size @n@.
--
-- The minimum chunk size is 1.  If the given value is less than 1,
-- a chunk size of 1 is used.
--
fromList :: Int -> [a] -> V a
fromList n = V . go
  where
  go [] = []
  go xs = let (h,t) = L.splitAt (max 1 n) xs in V.fromList h : go t

fromChunks :: [V.Vector a] -> V a
fromChunks = V

-- | /O(n\/c)/ Split the list at given index.
-- Chunk fragmentation can occur at the boundary.
splitAt :: Int -> V a -> (V a, V a)
splitAt i (V chunks) = case chunks of
  [] -> (V [], V [])
  (h:t)
    | i == length h -> (V [h], V t)
    | i < length h ->
      let (th, ht) = V.splitAt i h
      in (V [th], V (ht : t))
    | otherwise ->
      let (V h', V t') = splitAt (i - length h) (V t)
      in (V (h : h'), V t')

type instance Index (V a) = Int
type instance IxValue (V a) = a

instance Ixed (V a) where
  ix n = splitIso (n + 1) . _1 . _VIso . _last . _last
    where
    splitIso :: Int -> Iso' (V a) (V a, V a)
    splitIso i = iso (splitAt i) (uncurry (<>))

    -- type checker needs some help
    _VIso :: Iso' (V a) [V.Vector a]
    _VIso = coerced
