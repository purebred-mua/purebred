-- | storage related data types
{-# LANGUAGE TemplateHaskell #-}
module Storage.Mail where

import Control.Lens.TH (makeLenses)
import Data.Text (Text)

-- A single mail represented in the UI
-- TODO: should use Text instead of String
data Mail = Mail
    { _subject :: Text
    , _to :: Text
    , _from :: Text
    , _filepath :: String
    , _mailTags :: [Text]
    , _mailIsNew :: Bool
    } deriving (Show)

makeLenses ''Mail
