{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module TestActions where

import qualified Brick.Widgets.List as L

import Control.Lens
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Data.Vector as Vector

import Types
import UI.Actions
import UI.Utils (selectedFiles)
import UI.Views (swapWidget, findNextVisibleWidget)


actionTests ::
  TestTree
actionTests =
    testGroup
        "action tests"
        [ testModeDescription
        , testNoDupes
        , testSwapBottom
        , testFocusNext
        ]

testModeDescription :: TestTree
testModeDescription = testCase "mode present in the switch action"
                      $ view aDescription a @?= ["switch mode to ManageMailTagsEditor"]
  where
    a = focus :: Action 'Mails 'ManageMailTagsEditor AppState

testNoDupes :: TestTree
testNoDupes =
    let vec =
            Vector.fromList
                [ (True, File "file 1")
                , (True, File "file 2")
                , (False, File "file 3")]
        l = set L.listSelectedL (Just 1) $ L.list ListOfFiles vec 1
    in testCase "no duplicates when multiple items are selected" $
       selectedFiles l @?= ["file 1", "file 2"]

testFocusNext :: TestTree
testFocusNext = testCase "focuses next visible widget" $ findNextVisibleWidget ListOfThreads 1 tiles @?= SearchThreadsEditor
  where
    tiles =
        Vector.fromList
          [ Tile Visible ListOfThreads
          , Tile Visible StatusBar
          , Tile Visible SearchThreadsEditor
          , Tile Hidden ManageThreadTagsEditor
          , Tile Hidden ComposeFrom
          ]

testSwapBottom :: TestTree
testSwapBottom = testCase "swaps last visible widget" $ swapWidget ListOfThreads ManageThreadTagsEditor tiles @?= expected
  where
    tiles =
        Tiles $ Vector.fromList
          [ Tile Visible ListOfThreads
          , Tile Visible StatusBar
          , Tile Visible SearchThreadsEditor
          , Tile Hidden ManageThreadTagsEditor
          , Tile Hidden ComposeFrom
          ]
    expected =
        Tiles $ Vector.fromList
          [ Tile Visible ListOfThreads
          , Tile Visible StatusBar
          , Tile Hidden SearchThreadsEditor
          , Tile Visible ManageThreadTagsEditor
          , Tile Hidden ComposeFrom
          ]
