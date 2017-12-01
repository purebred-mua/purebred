{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module TestActions where

import Types
import UI.Actions

import Control.Lens (view)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

actionTests ::
  TestTree
actionTests =
    testGroup
        "action tests"
        [testModeDescription]

testModeDescription :: TestTree
testModeDescription = testCase "mode present in the switch action"
                      $ view aDescription a @?= "switch mode to ManageMailTags"
  where
    a = focus :: Action 'ManageMailTags AppState
