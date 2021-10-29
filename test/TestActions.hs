-- This file is part of purebred
-- Copyright (C) 2018 Róman Joost
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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module TestActions where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Data.Vector as Vector

import Types
import Purebred.UI.Views (swapWidget)


actionTests ::
  TestTree
actionTests = testGroup "action tests" [testSwapBottom]

testSwapBottom :: TestTree
testSwapBottom = testCase "swaps last visible widget" $ swapWidget ListOfThreads ManageThreadTagsEditor tiles @?= expected
  where
    tiles =
        Layer $ Vector.fromList
          [ Tile Visible ListOfThreads
          , Tile Visible StatusBar
          , Tile Visible SearchThreadsEditor
          , Tile Hidden ManageThreadTagsEditor
          , Tile Hidden ComposeFrom
          ]
    expected =
        Layer $ Vector.fromList
          [ Tile Visible ListOfThreads
          , Tile Visible StatusBar
          , Tile Hidden SearchThreadsEditor
          , Tile Visible ManageThreadTagsEditor
          , Tile Hidden ComposeFrom
          ]
