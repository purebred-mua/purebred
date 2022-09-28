-- This file is part of purebred
-- Copyright (C) 2019 Róman Joost
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

module TestTextParser where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit ((@=?), testCase)

import Purebred.Storage.Mail (parseMailbody)
import Purebred.Types

textparserTests ::
  TestTree
textparserTests = testParsesParagraphs

testParsesParagraphs :: TestTree
testParsesParagraphs =
  testCase "parses paragraphs" $ expected @=? parseMailbody 72 mempty paragraph
  where
    expected = MailBody mempty
      [ Line mempty "This is a"
      , Line mempty "Paragraph"
      , Line mempty "of Text"
      , Line mempty ""
      , Line mempty "And here"
      , Line mempty "comes another one."
      ]
    paragraph = "This is a\nParagraph\nof Text\n\nAnd here\ncomes another one."
