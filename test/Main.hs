module Main where

import TestMail (mailTests)
import TestUserAcceptance (systemTests)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests ::
  TestTree
tests = testGroup "tests" [unittests, systemTests]

-- unit tests
--
unittests :: TestTree
unittests =
    testGroup "unit tests" [mailTests]

main ::
  IO ()
main = defaultMain tests
