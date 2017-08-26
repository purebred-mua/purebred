{-# LANGUAGE OverloadedStrings #-}
module TestMail where

import Types (NotmuchMail(..))
import Storage.ParsedMail (parseMail)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (secondsToDiffTime, UTCTime(..))

mailTests ::
  TestTree
mailTests = testGroup "mail parsing tests" [testMailHasBeenMoved]

testMailHasBeenMoved ::
  TestTree
testMailHasBeenMoved = testCase "does not crash" $ do
  msg <- parseMail m
  Left "/path/does/not/exist: openFile: does not exist (No such file or directory)" @?= msg
  where
    m = NotmuchMail "" "" "" "/path/does/not/exist" t ["unread"] True
    t = UTCTime (fromGregorian 2017 7 7) (secondsToDiffTime 39292)
