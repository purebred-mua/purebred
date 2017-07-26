-- | storage related data types
{-# LANGUAGE TemplateHaskell #-}
module Storage.Mail where

import           Lens.Micro.Platform (makeLenses)

-- A single mail represented in the UI
-- TODO: should use Text instead of String
data Mail = Mail
    { _subject :: String
    , _to      :: String
    , _from    :: String
    } deriving (Show)

makeLenses ''Mail
