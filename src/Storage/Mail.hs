-- | storage related data types
{-# LANGUAGE TemplateHaskell #-}
module Storage.Mail where

import Control.Lens.TH (makeLenses)
import Data.Text (Text)
import Data.Time (UTCTime)

-- A single mail represented in the UI
-- TODO: should use Text instead of String
data Mail = Mail
    { _subject :: Text
    , _to :: Text
    , _from :: Text
    , _filepath :: String
    , _mailDate :: UTCTime
    , _mailTags :: [Text]
    , _mailIsNew :: Bool
    } deriving (Show)

makeLenses ''Mail
