{-# LANGUAGE OverloadedStrings #-}
module TestMail where

import Data.Text (Text, pack)
import Types (NotmuchMail(..))
import Storage.ParsedMail (parseMail)
import Storage.Notmuch (addTag, removeTag)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
       (testProperty, Arbitrary, arbitrary, listOf, choose, Gen)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.QuickCheck.Utf8 (utf8BS, genValidUtf8)
import Data.Text.Arbitrary ()
import Data.Time.Calendar (fromGregorian, Day(..))
import Data.Time.Clock (secondsToDiffTime, UTCTime(..), DiffTime)

mailTests ::
  TestTree
mailTests =
    testGroup
        "mail parsing tests"
        [testMailHasBeenMoved, testAddingTags, testRemovingTags]

testMailHasBeenMoved ::
  TestTree
testMailHasBeenMoved = testCase "does not crash" $ do
  msg <- parseMail m
  Left "/path/does/not/exist: openFile: does not exist (No such file or directory)" @?= msg
  where
    m = NotmuchMail "" "" "/path/does/not/exist" t ["unread"] "0815"
    t = UTCTime (fromGregorian 2017 7 7) (secondsToDiffTime 39292)


testAddingTags :: TestTree
testAddingTags = testProperty "no duplicates when adding tags" propNoDuplicatesAdded
  where
    propNoDuplicatesAdded :: NotmuchMail -> Text -> Bool
    propNoDuplicatesAdded m a = addTag (addTag m a) a == addTag m a

testRemovingTags = testProperty "remove tags" propRemoveTags
  where
    propRemoveTags :: NotmuchMail -> Text -> Bool
    propRemoveTags m a = removeTag (removeTag m a) a == removeTag m a

instance Arbitrary NotmuchMail where
    arbitrary =
        NotmuchMail <$>
        (pack <$> arbitrary) <*>
        (pack <$> arbitrary) <*>
        arbitrary <*>
        arbitrary <*>
        listOf genValidUtf8 <*>
        utf8BS

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> arbitrary <*> genDiffTime

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay <$> arbitrary

genDiffTime :: Gen DiffTime
genDiffTime = do
  i <- choose (0, 200000)
  pure $ secondsToDiffTime i
