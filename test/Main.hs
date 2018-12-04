module Main where

import qualified LazyVector
import TestMail (mailTests)
import TestUserAcceptance (systemTests)
import TestActions (actionTests)
import TestTagParser (tagparserTests)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests ::
  TestTree
tests = testGroup "tests" [unittests, systemTests]

-- unit tests
--
unittests :: TestTree
unittests = testGroup "unit tests"
  [ mailTests
  , tagparserTests
  , actionTests
  , LazyVector.tests
  ]

main ::
  IO ()
main = defaultMain tests
