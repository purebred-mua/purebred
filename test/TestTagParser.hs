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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module TestTagParser where

import Data.List (isInfixOf)

import qualified Data.Text as T
import Purebred.Tags
import Types (TagOp(..), UserMessage(..), Name(..), MessageSeverity(..))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@=?), testCase, assertBool, assertFailure)

tagparserTests ::
  TestTree
tagparserTests = testParseTags

testParseTags :: TestTree
testParseTags =
    let tagops =
            [ ("adding", (Right [AddTag "foo"] @=?), "+foo")
            , ("removing", (Right [RemoveTag "foo"] @=?), "-foo")
            , ("resetting", (Right [ResetTags] @=?), "=")
            , ( "mixed"
              , (Right [ResetTags, AddTag "foo", RemoveTag "foo"] @=?)
              , "= +foo -foo"
              )
            , ( "whitespace is handled gracefully"
              , (Right [ResetTags, AddTag "foo", RemoveTag "foo"] @=?)
              , "=+foo   -foo"
              )
            , ( "wrong order"
              , \case
                  Left (UserMessage StatusBar (Warning msg)) ->
                    assertBool "message indicates bad char"
                    $ "unexpected '='" `isInfixOf` T.unpack msg
                  Left e ->
                    assertFailure $ "parse failed with unexpected error: " <> show e
                  Right _ ->
                    assertFailure "parse succeeded but should have failed"
              , "+foo = -foo"
              )
            ]
    in testGroup "tag op parsing tests" $
       (\(desc, f, input) -> testCase desc $ f (parseTagOps input))
       <$> tagops
