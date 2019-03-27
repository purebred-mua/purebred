-- This file is part of purebred
-- Copyright (C) 2019 Fraser Tweedale
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

{- |

Information flow control types and functions.

-}
module Purebred.Types.IFC
  (
    Tainted
  , taint
  , untaint

  -- * Sanitisation functions
  , sanitiseText
  ) where

import Data.Char (chr, isControl, ord)

import qualified Data.Text as T

-- | A tainted value can only be unwrapped by applying 'untaint'
-- with a sanitisation function.  This approach is used instead of
-- type classes because how you untaint a value might depend on how
-- that value will be used.
--
-- You /could/ just use 'untaint id' to get the value out.
-- But you probably shouldn't.
--
newtype Tainted a = Tainted a

-- | Taint a value
taint :: a -> Tainted a
taint = Tainted

-- | Untaint a value.
untaint :: (a -> b) -> Tainted a -> b
untaint f (Tainted a) = f a

-- | Convert or strip control characters from input.
--
-- * Tab (HT) is replaced with 8 spaces.
-- * Other C0 codes (except CR and LF) and DEL are replaced with
--   <https://en.wikipedia.org/wiki/Control_Pictures Control Pictures>
-- * C1 and all other control characters are replaced with
--   REPLACEMENT CHARACTER U+FFFD
--
sanitiseText :: T.Text -> T.Text
sanitiseText = T.map substControl . T.replace "\t" "        "
  where
  substControl c
    | c == '\n' || c == '\r' = c  -- CR and LF are OK
    | c <= '\x1f' = chr (0x2400 + ord c)
    | c == '\DEL' = '\x2421'
    | isControl c = '\xfffd' -- REPLACEMENT CHARACTER
    | otherwise = c
