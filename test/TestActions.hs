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
{-# LANGUAGE TypeApplications #-}

module TestActions where

import qualified Brick.Widgets.List as L

import Control.Lens
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Data.Vector as Vector

import Types
import UI.Actions
import UI.Views (swapWidget)


actionTests ::
  TestTree
actionTests =
    testGroup
        "action tests"
        [ testModeDescription
        , testSwapBottom
        ]

testModeDescription :: TestTree
testModeDescription = testCase "mode present in the switch action"
                      $ view aDescription a @?= ["switch mode to ManageMailTagsEditor"]
  where
    a = focus @'ViewMail @'ManageMailTagsEditor

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
