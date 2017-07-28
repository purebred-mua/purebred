module Main where

import           TestMail   (mailTests)

import           Test.Tasty (TestTree, defaultMain, testGroup)

tests ::
  TestTree
tests = testGroup "tests" [unittests]

-- unit tests
--
unittests :: TestTree
unittests =
    testGroup "unit tests" [mailTests]

main ::
  IO ()
main = defaultMain tests
