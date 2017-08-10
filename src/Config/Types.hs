{-# LANGUAGE TemplateHaskell #-}
module Config.Types where

import qualified Brick.AttrMap  as A
import           Data.Text      (Text)
import           UI.Keybindings (Keybinding)

data IndexView = IndexView
    { _ivKeybindings :: [Keybinding]
    }

data MailView = MailView
    { _mvIndexRows           :: Int
    , _mvPreferedContentType :: Text
    , _mvHeadersToShow       :: [Text]
    }
makeLenses ''MailView

data Configuration = Configuration
    { _confColorMap        :: A.AttrMap
    , _confNotmuchsearch   :: Text
    , _confNotmuchDatabase :: String
    , _confEditor          :: Text
    , _confMailView        :: MailView
    , _confIndexView       :: IndexView
    }
