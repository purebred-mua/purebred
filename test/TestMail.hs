{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module TestMail where

import qualified Data.ByteString as B
import qualified Data.Text.Encoding as T
import Control.Lens (view)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar (fromGregorian)

import Test.Tasty.HUnit ((@=?), testCase)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
       (testProperty, Arbitrary, arbitrary, choose)
import Test.QuickCheck.Instances ()

import Notmuch (mkTag, tagMaxLen)

import Types
import Storage.Notmuch (addTags, removeTags, tagItem)

mailTests ::
  TestTree
mailTests =
    testGroup
        "mail parsing tests"
        [testAddingTags, testRemovingTags, testTagOpsWithReset]

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

testTagOpsWithReset :: TestTree
testTagOpsWithReset = testCase "tag ops with reset" $ ["archive"] @=? (view mailTags actual)
  where
    m = NotmuchMail "subject" "from" time ["foo", "bar"] "asdf"
    time = UTCTime (fromGregorian 2018 1 15) (secondsToDiffTime 123)
    actual = tagItem [ResetTags, AddTag "archive"] m

instance Arbitrary Tag where
  arbitrary = do
    n <- choose (1, tagMaxLen)
    bs <- fmap B.pack . sequenceA . replicate n $ choose (0x21, 0x7e)
    maybe arbitrary{-try again-} pure (mkTag bs)

instance Arbitrary NotmuchMail where
    arbitrary =
      NotmuchMail
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> (T.encodeUtf8 <$> arbitrary)
