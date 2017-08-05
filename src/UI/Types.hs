-- | Basic types for the UI used by this library
{-# LANGUAGE TemplateHaskell #-}
module UI.Types where

import           Brick.Types               (EventM, Next)
import qualified Brick.Widgets.Edit        as E
import qualified Brick.Widgets.List        as L
import           Codec.MIME.Type           (MIMEValue)
import           Control.Lens.TH           (makeLenses)
import qualified Data.Text                 as T
import qualified Graphics.Vty.Input.Events as Vty
import           Storage.Mail              (Mail)


-- | The global application mode
data Mode
    = Main  -- ^ focus is on the main screen
    | ViewMail  -- ^ focus is on the screen showing the entire mail
    | GatherHeaders  -- ^ focus is on the command line to gather input for composing an e-mail
    | ComposeEditor  -- ^ edit the final e-mail

-- | Used to identify widgets in brick
data Name =
    EditorInput
    | ListOfMails
    | ScrollingMailView
    | GatherHeadersFrom
    | GatherHeadersTo
    | GatherHeadersSubject
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

data ComposeState
    = AskFrom
    | AskTo
    | AskSubject
    deriving (Eq)

data Compose = Compose
    { _cTmpFile :: Maybe String
    , _cFocus   :: ComposeState
    , _cFrom    :: E.Editor T.Text Name
    , _cTo      :: E.Editor T.Text Name
    , _cSubject :: E.Editor T.Text Name
    }
makeLenses ''Compose

-- | Overall application state
data AppState = AppState
    { _asNotmuchRawsearch  :: String  -- ^ the raw database search entered by the user
    , _asNotmuchDatabaseFp :: String  -- ^ file path to the notmuch database
    , _asMailIndex         :: MailIndex
    , _asMailView          :: MailView
    , _asCompose           :: Compose  -- ^ state to keep when user creates a new mail
    , _asAppMode           :: Mode
    , _asError             :: Maybe String -- ^ in case of errors, show this error message
    }

makeLenses ''AppState

data Keybinding = Keybinding
    { _kbDescription :: String
    , _kbEvent       :: Vty.Event
    , _kbAction      :: AppState -> EventM Name (Next AppState)
    }
makeLenses ''Keybinding
