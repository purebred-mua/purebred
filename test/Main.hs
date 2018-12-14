module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified LazyVector
import TestMail (mailTests)
import TestActions (actionTests)
import TestTagParser (tagparserTests)

main :: IO ()
main = defaultMain $ testGroup "unit tests"
  [ mailTests
  , tagparserTests
  , actionTests
  , LazyVector.tests
  ]
