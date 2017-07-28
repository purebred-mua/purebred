-- | Basic types for the UI used by this library
{-# LANGUAGE TemplateHaskell #-}
module UI.Types where

import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import           Codec.MIME.Type    (MIMEValue)
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
    | ScrollingMailView
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
    { _miListOfMails  :: L.List Name Mail
    , _miSearchEditor :: E.Editor T.Text Name
    , _miMode         :: MainMode
    }
makeLenses ''MailIndex


type Body = T.Text
type Header = T.Text

-- | a parsed email representing either a MIME or RFC2822 e-mail. Note: RFC2822
-- is currently not implemented, but we're using the same type for the case we
-- add support for it
data ParsedMail
    = MIMEMail MIMEValue
    | RFC2822 [Header]
              Body
    deriving (Show,Eq)


data MailView = MailView
    { _mvMail :: Maybe ParsedMail
    }

makeLenses ''MailView

-- | Overall application state
data AppState = AppState
    { _asNotmuchRawsearch  :: String  -- ^ the raw database search entered by the user
    , _asNotmuchDatabaseFp :: String  -- ^ file path to the notmuch database
    , _asMailIndex         :: MailIndex
    , _asMailView          :: MailView
    , _asAppMode           :: Mode
    , _asError             :: Maybe String -- ^ in case of errors, show this error message
    }

makeLenses ''AppState
