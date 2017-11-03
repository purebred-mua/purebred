{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | Basic types for the UI used by this library
module Types where

import Codec.MIME.Type (MIMEValue)
import qualified Brick.AttrMap             as Brick
import Brick.Types (EventM, Next)
import qualified Brick.Widgets.Edit        as E
import qualified Brick.Widgets.List        as L
import           Control.Lens
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Graphics.Vty.Input.Events as Vty
import Data.Time (UTCTime)
import qualified Data.CaseInsensitive as CI

import Error

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}


-- | The global application mode
data Mode
    = BrowseMail   -- ^ input focus goes to navigating the list of mails
    | BrowseThreads  -- ^ input focus goes to navigating the list of threads
    | SearchThreads   -- ^ input focus goes to manipulating the notmuch search (main screen)
    | ViewMail   -- ^ focus is on the screen showing the entire mail
    | GatherHeadersFrom   -- ^ focus is on the command line to gather input for composing an e-mail
    | GatherHeadersTo   -- ^ focus is on the command line to gather input for composing an e-mail
    | GatherHeadersSubject   -- ^ focus is on the command line to gather input for composing an e-mail
    | ComposeEditor   -- ^ edit the final e-mail
    | Help  -- ^ shows all keybindings
    | ManageMailTags -- ^ add/remove tags on mails
    | ManageThreadTags -- ^ add/remove tags on threads
    deriving (Eq,Show,Ord)

-- | Used to identify widgets in brick
data Name =
    SearchThreadsEditor
    | ListOfMails
    | ListOfThreads
    | ScrollingMailView
    | ComposeFrom
    | ComposeTo
    | ComposeSubject
    | ScrollingHelpView
    | ManageMailTagsEditor
    | ManageThreadTagsEditor
    deriving (Eq,Show,Ord)

{- | main application interface

The main UI shows a list of e-mails, allows the user to manipulate the notmuch
search and composes e-mails from here.

-}
data MailIndex = MailIndex
    { _miListOfMails  :: L.List Name NotmuchMail
    , _miListOfThreads :: L.List Name NotmuchThread
    , _miSearchThreadsEditor :: E.Editor T.Text Name
    , _miMailTagsEditor :: E.Editor T.Text Name
    , _miThreadTagsEditor :: E.Editor T.Text Name
    }

miListOfMails :: Lens' MailIndex (L.List Name NotmuchMail)
miListOfMails = lens _miListOfMails (\m v -> m { _miListOfMails = v })

miListOfThreads :: Lens' MailIndex (L.List Name NotmuchThread)
miListOfThreads = lens _miListOfThreads (\m v -> m { _miListOfThreads = v})

miSearchThreadsEditor :: Lens' MailIndex (E.Editor T.Text Name)
miSearchThreadsEditor = lens _miSearchThreadsEditor (\m v -> m { _miSearchThreadsEditor = v})

miMailTagsEditor :: Lens' MailIndex (E.Editor T.Text Name)
miMailTagsEditor = lens _miMailTagsEditor (\m v -> m { _miMailTagsEditor = v})

miThreadTagsEditor :: Lens' MailIndex (E.Editor T.Text Name)
miThreadTagsEditor = lens _miThreadTagsEditor (\m v -> m { _miThreadTagsEditor = v})

data HeadersState = ShowAll | Filtered

data MailView = MailView
    { _mvMail :: Maybe ParsedMail
    , _mvHeadersState :: HeadersState
    }

mvMail :: Lens' MailView (Maybe ParsedMail)
mvMail = lens _mvMail (\mv pm -> mv { _mvMail = pm })

mvHeadersState :: Lens' MailView HeadersState
mvHeadersState = lens _mvHeadersState (\mv hs -> mv { _mvHeadersState = hs })

data Compose = Compose
    { _cTmpFile :: Maybe String
    , _cFrom    :: E.Editor T.Text Name
    , _cTo      :: E.Editor T.Text Name
    , _cSubject :: E.Editor T.Text Name
    }

cTmpFile :: Lens' Compose (Maybe String)
cTmpFile f (Compose a b c d) = fmap (\a' -> Compose a' b c d) (f a)

cFrom :: Lens' Compose (E.Editor T.Text Name)
cFrom f (Compose a b c d) = fmap (\b' -> Compose a b' c d) (f b)

cTo :: Lens' Compose (E.Editor T.Text Name)
cTo f (Compose a b c d) = fmap (\c' -> Compose a b c' d) (f c)

cSubject :: Lens' Compose (E.Editor T.Text Name)
cSubject f (Compose a b c d) = fmap (\d' -> Compose a b c d') (f d)


data NotmuchSettings a = NotmuchSettings
    { _nmSearch :: T.Text
    , _nmDatabase :: a
    , _nmNewTag :: T.Text
    }

nmSearch :: Lens' (NotmuchSettings a) T.Text
nmSearch f (NotmuchSettings a b c) = fmap (\a' -> NotmuchSettings a' b c) (f a)

nmDatabase :: Lens (NotmuchSettings a) (NotmuchSettings b) a b
nmDatabase f (NotmuchSettings a b c) = fmap (\b' -> NotmuchSettings a b' c) (f b)

nmNewTag :: Getter (NotmuchSettings a) T.Text
nmNewTag = to (\(NotmuchSettings _ _ c) -> c)

data Configuration a b = Configuration
    { _confColorMap :: Brick.AttrMap
    , _confNotmuch :: NotmuchSettings a
    , _confEditor :: b
    , _confMailView :: MailViewSettings
    , _confIndexView :: IndexViewSettings
    , _confComposeView :: ComposeViewSettings
    , _confHelpView :: HelpViewSettings
    }

type UserConfiguration = Configuration (IO FilePath) (IO String)
type InternalConfiguration = Configuration FilePath String

confColorMap :: Getter (Configuration a b) Brick.AttrMap
confColorMap = to (\(Configuration a _ _ _ _ _ _) -> a)

confEditor :: Lens (Configuration a b) (Configuration a b') b b'
confEditor f (Configuration a b c d e g h) = fmap (\c' -> Configuration a b c' d e g h) (f c)

confNotmuch :: Lens (Configuration a c) (Configuration b c) (NotmuchSettings a) (NotmuchSettings b)
confNotmuch f (Configuration a b c d e g h) = fmap (\b' -> Configuration a b' c d e g h) (f b)

confMailView :: Lens' (Configuration a b) MailViewSettings
confMailView f (Configuration a b c d e g h) = fmap (\d' -> Configuration a b c d' e g h) (f d)

confIndexView :: Lens' (Configuration a b) IndexViewSettings
confIndexView f (Configuration a b c d e g h) = fmap (\e' -> Configuration a b c d e' g h) (f e)

confComposeView :: Lens' (Configuration a b) ComposeViewSettings
confComposeView f (Configuration a b c d e g h) = fmap (\g' -> Configuration a b c d e g' h) (f g)

confHelpView :: Lens' (Configuration a b) HelpViewSettings
confHelpView f (Configuration a b c d e g h) = fmap (\h' -> Configuration a b c d e g h') (f h)

data ComposeViewSettings = ComposeViewSettings
    { _cvKeybindings :: [Keybinding 'ComposeEditor (Next AppState)]
    , _cvFromKeybindings :: [Keybinding 'GatherHeadersFrom (Next AppState)]
    , _cvToKeybindings :: [Keybinding 'GatherHeadersTo (Next AppState)]
    , _cvSubjectKeybindings :: [Keybinding 'GatherHeadersSubject (Next AppState)]
    }

cvKeybindings :: Lens' ComposeViewSettings [Keybinding 'ComposeEditor (Next AppState)]
cvKeybindings = lens _cvKeybindings (\cv x -> cv { _cvKeybindings = x })

cvFromKeybindings :: Lens' ComposeViewSettings [Keybinding 'GatherHeadersFrom (Next AppState)]
cvFromKeybindings = lens _cvFromKeybindings (\cv x -> cv { _cvFromKeybindings = x })

cvToKeybindings :: Lens' ComposeViewSettings [Keybinding 'GatherHeadersTo (Next AppState)]
cvToKeybindings = lens _cvToKeybindings (\cv x -> cv { _cvToKeybindings = x })

cvSubjectKeybindings :: Lens' ComposeViewSettings [Keybinding 'GatherHeadersSubject (Next AppState)]
cvSubjectKeybindings = lens _cvSubjectKeybindings (\cv x -> cv { _cvSubjectKeybindings = x })

newtype HelpViewSettings = HelpViewSettings
  { _hvKeybindings :: [Keybinding 'Help (Next AppState)]
  }

hvKeybindings :: Lens' HelpViewSettings [Keybinding 'Help (Next AppState)]
hvKeybindings f (HelpViewSettings a) = fmap (\a' -> HelpViewSettings a') (f a)

data IndexViewSettings = IndexViewSettings
    { _ivBrowseThreadsKeybindings :: [Keybinding 'BrowseThreads (Next AppState)]
    , _ivBrowseMailsKeybindings :: [Keybinding 'BrowseMail (Next AppState)]
    , _ivSearchThreadsKeybindings :: [Keybinding 'SearchThreads (Next AppState)]
    , _ivManageMailTagsKeybindings :: [Keybinding 'ManageMailTags (Next AppState)]
    , _ivManageThreadTagsKeybindings :: [Keybinding 'ManageThreadTags (Next AppState)]
    }

ivBrowseThreadsKeybindings :: Lens' IndexViewSettings [Keybinding 'BrowseThreads (Next AppState)]
ivBrowseThreadsKeybindings f (IndexViewSettings a b c d e) = fmap (\a' -> IndexViewSettings a' b c d e) (f a)

ivBrowseMailsKeybindings :: Lens' IndexViewSettings [Keybinding 'BrowseMail (Next AppState)]
ivBrowseMailsKeybindings f (IndexViewSettings a b c d e) = fmap (\b' -> IndexViewSettings a b' c d e) (f b)

ivSearchThreadsKeybindings :: Lens' IndexViewSettings [Keybinding 'SearchThreads (Next AppState)]
ivSearchThreadsKeybindings f (IndexViewSettings a b c d e) = fmap (\c' -> IndexViewSettings a b c' d e) (f c)

ivManageMailTagsKeybindings :: Lens' IndexViewSettings [Keybinding 'ManageMailTags (Next AppState)]
ivManageMailTagsKeybindings f (IndexViewSettings a b c d e) = fmap (\d' -> IndexViewSettings a b c d' e) (f d)

ivManageThreadTagsKeybindings :: Lens' IndexViewSettings [Keybinding 'ManageThreadTags (Next AppState)]
ivManageThreadTagsKeybindings f (IndexViewSettings a b c d e) = fmap (\e' -> IndexViewSettings a b c d e') (f e)

data MailViewSettings = MailViewSettings
    { _mvIndexRows           :: Int
    , _mvPreferedContentType :: T.Text
    , _mvHeadersToShow       :: CI.CI T.Text -> Bool
    , _mvKeybindings         :: [Keybinding 'ViewMail (Next AppState)]
    , _mvIndexKeybindings    :: [Keybinding 'BrowseMail (Next AppState)]
    }

mvIndexRows :: Lens' MailViewSettings Int
mvIndexRows f (MailViewSettings a b c d e) = fmap (\a' -> MailViewSettings a' b c d e) (f a)

mvPreferredContentType :: Lens' MailViewSettings T.Text
mvPreferredContentType f (MailViewSettings a b c d e) = fmap (\b' -> MailViewSettings a b' c d e) (f b)

mvHeadersToShow :: Getter MailViewSettings (CI.CI T.Text -> Bool)
mvHeadersToShow = to (\(MailViewSettings _ _ h _ _) -> h)

mvKeybindings :: Lens' MailViewSettings [Keybinding 'ViewMail (Next AppState)]
mvKeybindings f (MailViewSettings a b c d e) = fmap (\d' -> MailViewSettings a b c d' e) (f d)

mvIndexKeybindings :: Lens' MailViewSettings [Keybinding 'BrowseMail (Next AppState)]
mvIndexKeybindings f (MailViewSettings a b c d e) = fmap (\e' -> MailViewSettings a b c d e') (f e)

-- | Overall application state
data AppState = AppState
    { _asConfig    :: InternalConfiguration
    , _asMailIndex :: MailIndex
    , _asMailView  :: MailView
    , _asCompose   :: Compose  -- ^ state to keep when user creates a new mail
    , _asAppMode   :: Mode
    , _asError     :: Maybe Error -- ^ in case of errors, show this error message
    }

asConfig :: Lens' AppState InternalConfiguration
asConfig f (AppState a b c d e g) = fmap (\a' -> AppState a' b c d e g) (f a)

asMailIndex :: Lens' AppState MailIndex
asMailIndex f (AppState a b c d e g) = fmap (\b' -> AppState a b' c d e g) (f b)

asMailView :: Lens' AppState MailView
asMailView f (AppState a b c d e g) = fmap (\c' -> AppState a b c' d e g) (f c)

asCompose :: Lens' AppState Compose
asCompose f (AppState a b c d e g) = fmap (\d' -> AppState a b c d' e g) (f d)

asAppMode :: Lens' AppState Mode
asAppMode f (AppState a b c d e g) = fmap (\e' -> AppState a b c d e' g) (f e)

asError :: Lens' AppState (Maybe Error)
asError f (AppState a b c d e g) = fmap (\g' -> AppState a b c d e g') (f g)

data Action (ctx :: Mode) a = Action
    { _aDescription :: String
    , _aAction :: AppState -> EventM Name a
    }

aAction :: Getter (Action ctx a) (AppState -> EventM Name a)
aAction = to (\(Action _ b) -> b)

data Keybinding (ctx :: Mode) a = Keybinding
    { _kbEvent :: Vty.Event
    , _kbAction :: Action ctx a
    }
instance Eq (Keybinding ctx a) where
  (==) (Keybinding a _) (Keybinding b _) = a == b
  (/=) (Keybinding a _) (Keybinding b _) = a /= b

kbEvent :: Getter (Keybinding ctx a) Vty.Event
kbEvent = to (\(Keybinding b _) -> b)

kbAction :: Getter (Keybinding ctx a) (Action ctx a)
kbAction = to (\(Keybinding _ c) -> c)

aDescription :: Getter (Action ctx a) String
aDescription = to (\(Action a _ ) -> a)

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

-- | an email from the notmuch database
data NotmuchMail = NotmuchMail
    { _mailSubject :: T.Text
    , _mailFrom :: T.Text
    , _mailDate :: UTCTime
    , _mailTags :: [T.Text]
    , _mailId :: ByteString
    } deriving (Show, Eq)

mailSubject :: Lens' NotmuchMail T.Text
mailSubject = lens _mailSubject (\m s -> m { _mailSubject = s })

mailFrom :: Lens' NotmuchMail T.Text
mailFrom = lens _mailFrom (\m f -> m { _mailFrom = f })

mailDate :: Lens' NotmuchMail UTCTime
mailDate = lens _mailDate (\m d -> m { _mailDate = d })

mailTags :: Lens' NotmuchMail [T.Text]
mailTags = lens _mailTags (\m t -> m { _mailTags = t })

mailId :: Lens' NotmuchMail ByteString
mailId = lens _mailId (\m i -> m { _mailId = i })

data NotmuchThread = NotmuchThread
    { _thSubject :: T.Text
    , _thAuthors :: [T.Text]
    , _thDate :: UTCTime
    , _thTags :: [T.Text]
    , _thReplies :: Int
    , _thId :: ByteString
    } deriving (Show, Eq)

thSubject :: Lens' NotmuchThread T.Text
thSubject = lens _thSubject (\m s -> m { _thSubject = s })

thAuthors :: Lens' NotmuchThread [T.Text]
thAuthors = lens _thAuthors (\m f -> m { _thAuthors = f })

thDate :: Lens' NotmuchThread UTCTime
thDate = lens _thDate (\m d -> m { _thDate = d })

thTags :: Lens' NotmuchThread [T.Text]
thTags = lens _thTags (\m t -> m { _thTags = t })

thReplies :: Lens' NotmuchThread Int
thReplies = lens _thReplies (\m t -> m { _thReplies = t })

thId :: Lens' NotmuchThread ByteString
thId = lens _thId (\m t -> m { _thId = t })
