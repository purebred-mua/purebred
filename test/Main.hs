module Main where

import TestMail (mailTests)
import TestUserAcceptance (systemTests)
import TestActions (actionTests)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests ::
  TestTree
tests = testGroup "tests" [unittests, systemTests]

-- unit tests
--
unittests :: TestTree
unittests =
    testGroup "unit tests" [mailTests, actionTests]

main ::
  IO ()
main = defaultMain tests
