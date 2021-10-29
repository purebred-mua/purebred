-- This file is part of purebred
-- Copyright (C) 2017-2021 Fraser Tweedale and Róman Joost
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

module Purebred.Types.Parser.ByteString
  (
    niceEndOfInput
  , skipSpaces
  ) where

import Control.Applicative ((<|>))

import Data.Attoparsec.ByteString.Char8
  ( Parser, endOfInput, peekChar', skipMany1, space )
import qualified Data.Attoparsec.Internal.Types as AT

-- | Assert end of input has been reached, or fail with a message
-- that includes the problematic character and the offset.
niceEndOfInput :: Parser ()
niceEndOfInput = endOfInput <|> p
  where
  p = do
    c <- peekChar'
    off <- offset
    fail $ "unexpected " <> show c <> " at offset " <> show off

-- | Get the current position of the parser
offset :: AT.Parser i Int
offset = AT.Parser $ \t pos more _lose suc -> suc t pos more (AT.fromPos pos)

-- | skip whitespace.  fails on no whitespace
skipSpaces :: Parser ()
skipSpaces = skipMany1 space
