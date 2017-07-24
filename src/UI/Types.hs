{-# LANGUAGE TemplateHaskell #-}
module UI.Types where

import qualified Brick.Widgets.Edit  as E
import qualified Brick.Widgets.List  as L
import qualified Data.Text           as T
import           Lens.Micro.Platform (makeLenses)
import           Storage.Mail        (Mail)

-- | The mode in which the application is in
data Mode
    = Main
    | ViewMail

-- | Used to identify widgets in brick
data Name =
    EditorInput
    | ListOfMails
    deriving (Eq,Show,Ord)

-- | Modes for the main window
data MainMode
    = BrowseMail  -- ^ input focus goes to navigating the list of mails
    | SearchMail  -- ^ input focus goes to manipulating the notmuch search

-- | main application interface showing list of e-mails and an editor to
-- manipulate the notmuch search
data MailIndex = MailIndex
    { _listOfMails  :: L.List Name Mail
    , _searchEditor :: E.Editor T.Text Name
    , _miMode :: MainMode
    }
makeLenses ''MailIndex

-- | Overall application state
data AppState = AppState
    { _notmuchRawsearch  :: String  -- ^ the raw database search entered by the user
    , _notmuchDatabaseFp :: String  -- ^ file path to the notmuch database
    , _mailIndex         :: MailIndex
    , _appMode           :: Mode
    }

makeLenses ''AppState
