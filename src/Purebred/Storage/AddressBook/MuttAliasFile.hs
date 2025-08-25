-- This file is part of purebred
-- Copyright (C) 2022 - Róman Joost
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

module Purebred.Storage.AddressBook.MuttAliasFile
  ( -- * Synopsis
    -- $synopsis

    -- * API
    initMuttAliasFileAddressBook

    -- * Muttalias Parser
  , MuttAlias(..)
  , muttAliasNick
  , muttAliasAddress
  , parseMuttAliasFile
  ) where

import Data.Attoparsec.ByteString (Parser, parseOnly, sepBy, string, takeTill)
import Data.Attoparsec.ByteString.Char8 (endOfLine, isSpace_w8, skipSpace, space)
import Data.Bifunctor (bimap)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Internal.Search as T
import Control.Lens (Lens', lens, toListOf, folded, filtered, view)

import Data.MIME (defaultCharsets)
import Data.IMF (address, Address)

import Purebred.Types.Error (Error(ParseError))
import Purebred.Types.AddressBook (AddressBook(..))
import Purebred.Types.String (decodeLenient)

-- $synopsis
--
-- This module provides functions to retrieve addresses from an alias
-- file formatted in an mutt-alias compatible format.
--
-- https://gitlab.com/muttmua/mutt/-/wikis/MuttGuide/Aliases

initMuttAliasFileAddressBook :: FilePath -> IO (Either Error AddressBook)
initMuttAliasFileAddressBook filePath = do
  contents <- B.readFile filePath
  let mk addrs = AddressBook
        (\substr -> pure $ filterMuttAliases substr addrs)
        Nothing
  pure $ bimap ParseError mk $ parseMuttAliasFile contents

filterMuttAliases :: T.Text -> [MuttAlias] -> [Address]
filterMuttAliases substr =
  toListOf
    ( folded
        . filtered (matchesSubstring substr . view muttAliasNick)
        . muttAliasAddress
    )

matchesSubstring :: T.Text -> T.Text -> Bool
matchesSubstring needle haystack = not $ null $ T.indices needle haystack

-- | Parser functions to parse a mutt alias file
parseMuttAliasFile :: B.ByteString -> Either String [MuttAlias]
parseMuttAliasFile = parseOnly (muttalias `sepBy` endOfLine)

muttalias :: Parser MuttAlias
muttalias = do
 nick <- string "alias" *> space *> takeTill isSpace_w8
 add <- skipSpace *> address defaultCharsets
 pure $ MuttAlias (decodeLenient nick) add

-- | Parser Datatypes
data MuttAlias = MuttAlias
  { _muttAliasNick :: T.Text,
    _muttAliasAddress :: Address
  } deriving (Show, Eq)

muttAliasNick :: Lens' MuttAlias T.Text
muttAliasNick = lens _muttAliasNick (\m x -> m { _muttAliasNick = x })

muttAliasAddress :: Lens' MuttAlias Address
muttAliasAddress = lens _muttAliasAddress (\m x -> m { _muttAliasAddress = x })
