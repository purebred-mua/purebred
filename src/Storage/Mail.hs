module Storage.Mail where

-- A single mail represented in the UI
-- TODO: should use Text instead of String
data Mail = Mail { subject :: String
                 , to :: String
                 , from :: String
                 } deriving Show
