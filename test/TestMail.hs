{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module TestMail where

import qualified Data.ByteString as B
import Data.Text (pack)
import Types (NotmuchMail(..), Tag)
import Storage.Notmuch (addTags, removeTags)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
       (testProperty, Arbitrary, arbitrary, choose, Gen)
import Test.QuickCheck.Utf8 (utf8BS)
import Data.Text.Arbitrary ()
import Data.Time.Calendar (Day(..))
import Data.Time.Clock (secondsToDiffTime, UTCTime(..), DiffTime)

import Notmuch (mkTag, tagMaxLen)

mailTests ::
  TestTree
mailTests =
    testGroup
        "mail parsing tests"
        [testAddingTags, testRemovingTags]

testAddingTags :: TestTree
testAddingTags = testProperty "no duplicates when adding tags" propNoDuplicatesAdded
  where
    propNoDuplicatesAdded :: NotmuchMail -> [Tag] -> Bool
    propNoDuplicatesAdded m as = addTags as (addTags as m) == addTags as m

testRemovingTags :: TestTree
testRemovingTags = testProperty "remove tags" propRemoveTags
  where
    propRemoveTags :: NotmuchMail -> [Tag] -> Bool
    propRemoveTags m as = removeTags as (removeTags as m) == removeTags as m

instance Arbitrary Tag where
  arbitrary = do
    n <- choose (1, tagMaxLen)
    bs <- fmap B.pack . sequenceA . replicate n $ choose (0x21, 0x7e)
    maybe arbitrary{-try again-} pure (mkTag bs)

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
