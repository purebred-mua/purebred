-- This file is part of purebred
-- Copyright (C) 2022 Róman Joost
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

module TestAddressBook (
  addressbookTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Data.IMF (Address(..))

import Purebred.Storage.AddressBook.MuttAliasFile

addressbookTests :: TestTree
addressbookTests = testGroup "addressbook tests"
  [ testParseMuttAlias
  ]

testParseMuttAlias :: TestTree
testParseMuttAlias =
  testGroup
    "parse mutt alias file"
    [
      testCase "no long names" $
        parseMuttAliasFile "alias nick1 <nick1@test.example>\nalias nick2 nick2@foo.test"
          @?= Right [
          MuttAlias {
              _muttAliasNick = "nick1"
            , _muttAliasAddress = Single "nick1@test.example"
            }
        , MuttAlias {
              _muttAliasNick = "nick2"
            , _muttAliasAddress = Single "nick2@foo.test"
        }
      ]
    , testCase "with long names" $
        parseMuttAliasFile "alias nick1 Mr Nick Name <nick1@test.example>\nalias nick2 Nick Test Name <nick2@foo.test>"
          @?= Right [
          MuttAlias {
              _muttAliasNick = "nick1"
            , _muttAliasAddress = Single "Mr Nick Name <nick1@test.example>"
            }
        , MuttAlias {
              _muttAliasNick = "nick2"
            , _muttAliasAddress = Single "Nick Test Name <nick2@foo.test>"
            }
        ]
    ]
