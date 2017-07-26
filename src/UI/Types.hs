-- | Basic types for the UI used by this library
{-# LANGUAGE TemplateHaskell #-}
module UI.Types where

import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import           Control.Lens.TH    (makeLenses)
import qualified Data.Text          as T
import           Storage.Mail       (Mail)

-- | The global application mode
data Mode
    = Main  -- ^ focus is on the main screen
    | ViewMail  -- ^ focus is on the screen showing the entire mail

-- | Used to identify widgets in brick
data Name =
    EditorInput
    | ListOfMails
    deriving (Eq,Show,Ord)

-- | Modes for the main window to distinguish focus
data MainMode
    = BrowseMail  -- ^ input focus goes to navigating the list of mails
    | SearchMail  -- ^ input focus goes to manipulating the notmuch search

{- | main application interface

The main UI shows a list of e-mails, allows the user to manipulate the notmuch
search and composes e-mails from here.

-}
data MailIndex = MailIndex
    { _listOfMails  :: L.List Name Mail  -- ^ widget displaying a list of e-mails
    , _searchEditor :: E.Editor T.Text Name  -- ^ the input widget to manipulate the notmuch search
    , _miMode       :: MainMode  -- ^ mode to distinguish which widget should receive user input
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
