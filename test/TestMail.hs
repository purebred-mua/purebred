{-# LANGUAGE OverloadedStrings #-}
module TestMail where

import Data.Text (Text, pack)
import Types (NotmuchMail(..))
import Storage.Notmuch (addTag, removeTag)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
       (testProperty, Arbitrary, arbitrary, choose, Gen)
import Test.QuickCheck.Utf8 (utf8BS)
import Data.Text.Arbitrary ()
import Data.Time.Calendar (Day(..))
import Data.Time.Clock (secondsToDiffTime, UTCTime(..), DiffTime)

mailTests ::
  TestTree
mailTests =
    testGroup
        "mail parsing tests"
        [testAddingTags, testRemovingTags]

testAddingTags :: TestTree
testAddingTags = testProperty "no duplicates when adding tags" propNoDuplicatesAdded
  where
    propNoDuplicatesAdded :: NotmuchMail -> Text -> Bool
    propNoDuplicatesAdded m a = addTag (addTag m a) a == addTag m a

testRemovingTags :: TestTree
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
        utf8BS

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> arbitrary <*> genDiffTime

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay <$> arbitrary

genDiffTime :: Gen DiffTime
genDiffTime = do
  i <- choose (0, 200000)
  pure $ secondsToDiffTime i
