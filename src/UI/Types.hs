{-# LANGUAGE TemplateHaskell #-}
module UI.Types where

import qualified Brick.Widgets.List  as L
import           Lens.Micro.Platform (makeLenses)
import           Storage.Mail        (Mail)

-- | The mode in which the application is in
data Mode
    = Main
    | ViewMail

-- | Overall application state
data AppState = AppState
    { _notmuchRawsearch :: String
    , _mailIndex        :: L.List () Mail
    , _appMode          :: Mode
    }

makeLenses ''AppState
