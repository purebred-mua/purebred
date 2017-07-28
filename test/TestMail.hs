{-# LANGUAGE OverloadedStrings #-}
module TestMail where

import           Storage.Mail       (Mail (..))
import           Storage.ParsedMail (parseMail)
import           Test.Tasty         (TestTree, testGroup)
import           Test.Tasty.HUnit   (testCase, (@?=))

mailTests ::
  TestTree
mailTests = testGroup "mail parsing tests" [testMailHasBeenMoved]

testMailHasBeenMoved ::
  TestTree
testMailHasBeenMoved = testCase "does not crash" $ do
  msg <- parseMail m
  Left "/path/does/not/exist: openFile: does not exist (No such file or directory)" @?= msg
  where
    m = Mail "" "" "" "/path/does/not/exist"
