{-# LANGUAGE TemplateHaskell #-}
module Config.Types where

import qualified Brick.AttrMap   as A
import           Control.Lens.TH (makeLenses)
import           Data.Text       (Text)

data Configuration = Configuration
    { _confColorMap        :: A.AttrMap
    , _confNotmuchsearch   :: Text
    , _confNotmuchDatabase :: String
    , _confEditor          :: Text
    }
makeLenses ''Configuration

