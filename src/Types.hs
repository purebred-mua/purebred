{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

-- | Basic types for the UI used by this library
module Types
  ( module Types
  , Tag
  ) where

import Data.Semigroup (Semigroup, (<>))
import qualified Brick.Focus as Brick
import Brick.Themes (Theme)
import Brick.Widgets.Core ((<=>), emptyWidget)
import Brick.Types (EventM, Next, Widget)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Control.Lens
import qualified Data.Map as Map
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Graphics.Vty.Input.Events as Vty
import Data.Time (UTCTime)
import qualified Data.CaseInsensitive as CI

import Notmuch (Tag)
import Data.MIME

import Error

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}

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
    | ListOfAttachments
    | ListOfFiles
    | ManageFileBrowserSearchPath
    | StatusBar
    deriving (Eq,Show,Ord)

-- | Drawing types
newtype VBox = VBox { unVBox :: Widget Name }

instance Semigroup VBox where
  VBox a <> VBox b = VBox (a <=> b)

instance Monoid VBox where
  mappend = (<>)
  mempty = VBox emptyWidget

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
    { _mvMail :: Maybe MIMEMessage
    , _mvHeadersState :: HeadersState
    }

mvMail :: Lens' MailView (Maybe MIMEMessage)
mvMail = lens _mvMail (\mv pm -> mv { _mvMail = pm })

mvHeadersState :: Lens' MailView HeadersState
mvHeadersState = lens _mvHeadersState (\mv hs -> mv { _mvHeadersState = hs })

data Compose = Compose
    { _cMail :: B.ByteString
    , _cFrom :: E.Editor T.Text Name
    , _cTo :: E.Editor T.Text Name
    , _cSubject :: E.Editor T.Text Name
    , _cAttachments :: L.List Name MIMEMessage
    }

cMail :: Lens' Compose B.ByteString
cMail = lens _cMail (\c x -> c { _cMail = x })

cFrom :: Lens' Compose (E.Editor T.Text Name)
cFrom = lens _cFrom (\c x -> c { _cFrom = x })

cTo :: Lens' Compose (E.Editor T.Text Name)
cTo = lens _cTo (\c x -> c { _cTo = x })

cSubject :: Lens' Compose (E.Editor T.Text Name)
cSubject = lens _cSubject (\c x -> c { _cSubject = x })

cAttachments :: Lens' Compose (L.List Name MIMEMessage)
cAttachments = lens _cAttachments (\c x -> c { _cAttachments = x })

data NotmuchSettings a = NotmuchSettings
    { _nmSearch :: T.Text
    , _nmDatabase :: a
    , _nmNewTag :: Tag
    }

nmSearch :: Lens' (NotmuchSettings a) T.Text
nmSearch f (NotmuchSettings a b c) = fmap (\a' -> NotmuchSettings a' b c) (f a)

nmDatabase :: Lens (NotmuchSettings a) (NotmuchSettings b) a b
nmDatabase f (NotmuchSettings a b c) = fmap (\b' -> NotmuchSettings a b' c) (f b)

nmNewTag :: Lens' (NotmuchSettings a) Tag
nmNewTag f (NotmuchSettings a b c) = fmap (\c' -> NotmuchSettings a b c') (f c)


data FileBrowserSettings a = FileBrowserSettings
  { _fbKeybindings :: [Keybinding 'FileBrowser 'ListOfFiles]
  , _fbSearchPathKeybindings :: [Keybinding 'FileBrowser 'ManageFileBrowserSearchPath]
  , _fbHomePath :: a
  }

fbKeybindings :: Lens' (FileBrowserSettings a) [Keybinding 'FileBrowser 'ListOfFiles]
fbKeybindings = lens _fbKeybindings (\cv x -> cv { _fbKeybindings = x })

fbSearchPathKeybindings :: Lens' (FileBrowserSettings a) [Keybinding 'FileBrowser 'ManageFileBrowserSearchPath]
fbSearchPathKeybindings = lens _fbSearchPathKeybindings (\cv x -> cv { _fbSearchPathKeybindings = x})

fbHomePath :: Lens (FileBrowserSettings a) (FileBrowserSettings a') a a'
fbHomePath = lens _fbHomePath (\s a -> s { _fbHomePath = a })

data Configuration a b c = Configuration
    { _confTheme :: Theme
    , _confNotmuch :: NotmuchSettings a
    , _confEditor :: b
    , _confMailView :: MailViewSettings
    , _confIndexView :: IndexViewSettings
    , _confComposeView :: ComposeViewSettings
    , _confHelpView :: HelpViewSettings
    , _confDefaultView :: ViewName
    , _confFileBrowserView :: FileBrowserSettings c
    }

type UserConfiguration = Configuration (IO FilePath) (IO String) (IO FilePath)
type InternalConfiguration = Configuration FilePath String FilePath

type ConfigurationLens v = forall a b c. Lens' (Configuration a b c) v

confTheme :: ConfigurationLens Theme
confTheme = lens _confTheme (\c x -> c { _confTheme = x })

confEditor :: Lens (Configuration a b c) (Configuration a b' c) b b'
confEditor f (Configuration a b c d e g h i j) = fmap (\c' -> Configuration a b c' d e g h i j) (f c)

confNotmuch :: Lens (Configuration a b c) (Configuration a' b c) (NotmuchSettings a) (NotmuchSettings a')
confNotmuch f (Configuration a b c d e g h i j) = fmap (\b' -> Configuration a b' c d e g h i j) (f b)

confMailView :: ConfigurationLens MailViewSettings
confMailView f (Configuration a b c d e g h i j) = fmap (\d' -> Configuration a b c d' e g h i j) (f d)

confIndexView :: ConfigurationLens IndexViewSettings
confIndexView f (Configuration a b c d e g h i j) = fmap (\e' -> Configuration a b c d e' g h i j) (f e)

confComposeView :: ConfigurationLens ComposeViewSettings
confComposeView f (Configuration a b c d e g h i j) = fmap (\g' -> Configuration a b c d e g' h i j) (f g)

confHelpView :: ConfigurationLens HelpViewSettings
confHelpView f (Configuration a b c d e g h i j) = fmap (\h' -> Configuration a b c d e g h' i j) (f h)

confDefaultView :: ConfigurationLens ViewName
confDefaultView = lens _confDefaultView (\conf x -> conf { _confDefaultView = x })

confFileBrowserView :: Lens (Configuration a b c) (Configuration a b c') (FileBrowserSettings c) (FileBrowserSettings c')
confFileBrowserView = lens _confFileBrowserView (\conf x -> conf { _confFileBrowserView = x })

data ComposeViewSettings = ComposeViewSettings
    { _cvFromKeybindings :: [Keybinding 'ComposeView 'ComposeFrom]
    , _cvToKeybindings :: [Keybinding 'ComposeView 'ComposeTo]
    , _cvSubjectKeybindings :: [Keybinding 'ComposeView 'ComposeSubject]
    , _cvSendMailCmd :: B.ByteString -> IO String
    , _cvListOfAttachmentsKeybindings :: [Keybinding 'ComposeView 'ListOfAttachments]
    , _cvBoundary :: String
    , _cvIdentities :: [Mailbox]
    }

cvFromKeybindings :: Lens' ComposeViewSettings [Keybinding 'ComposeView 'ComposeFrom]
cvFromKeybindings = lens _cvFromKeybindings (\cv x -> cv { _cvFromKeybindings = x })

cvToKeybindings :: Lens' ComposeViewSettings [Keybinding 'ComposeView 'ComposeTo]
cvToKeybindings = lens _cvToKeybindings (\cv x -> cv { _cvToKeybindings = x })

cvSubjectKeybindings :: Lens' ComposeViewSettings [Keybinding 'ComposeView 'ComposeSubject]
cvSubjectKeybindings = lens _cvSubjectKeybindings (\cv x -> cv { _cvSubjectKeybindings = x })

cvSendMailCmd :: Lens' ComposeViewSettings (B.ByteString -> IO String)
cvSendMailCmd = lens _cvSendMailCmd (\cv x -> cv { _cvSendMailCmd = x })

cvListOfAttachmentsKeybindings :: Lens' ComposeViewSettings [Keybinding 'ComposeView 'ListOfAttachments]
cvListOfAttachmentsKeybindings = lens _cvListOfAttachmentsKeybindings (\cv x -> cv { _cvListOfAttachmentsKeybindings = x })

cvBoundary :: Lens' ComposeViewSettings String
cvBoundary = lens _cvBoundary (\cv x -> cv { _cvBoundary = x })

cvIdentities :: Lens' ComposeViewSettings [Mailbox]
cvIdentities = lens _cvIdentities (\cv x -> cv { _cvIdentities = x })

newtype HelpViewSettings = HelpViewSettings
  { _hvKeybindings :: [Keybinding 'Help 'ScrollingHelpView]
  }

hvKeybindings :: Lens' HelpViewSettings [Keybinding 'Help 'ScrollingHelpView]
hvKeybindings f (HelpViewSettings a) = fmap (\a' -> HelpViewSettings a') (f a)

data IndexViewSettings = IndexViewSettings
    { _ivBrowseThreadsKeybindings :: [Keybinding 'Threads 'ListOfThreads]
    , _ivBrowseMailsKeybindings :: [Keybinding 'Mails 'ListOfMails]
    , _ivSearchThreadsKeybindings :: [Keybinding 'Threads 'SearchThreadsEditor]
    , _ivManageMailTagsKeybindings :: [Keybinding 'Mails 'ManageMailTagsEditor]
    , _ivManageThreadTagsKeybindings :: [Keybinding 'Threads 'ManageThreadTagsEditor]
    , _ivFromKeybindings :: [Keybinding 'Threads 'ComposeFrom]
    , _ivToKeybindings :: [Keybinding 'Threads 'ComposeTo]
    , _ivSubjectKeybindings :: [Keybinding 'Threads 'ComposeSubject]
    }

ivBrowseThreadsKeybindings :: Lens' IndexViewSettings [Keybinding 'Threads 'ListOfThreads]
ivBrowseThreadsKeybindings = lens _ivBrowseThreadsKeybindings (\s x -> s { _ivBrowseThreadsKeybindings = x })

ivBrowseMailsKeybindings :: Lens' IndexViewSettings [Keybinding 'Mails 'ListOfMails]
ivBrowseMailsKeybindings = lens _ivBrowseMailsKeybindings (\s x -> s { _ivBrowseMailsKeybindings = x })

ivSearchThreadsKeybindings :: Lens' IndexViewSettings [Keybinding 'Threads 'SearchThreadsEditor]
ivSearchThreadsKeybindings = lens _ivSearchThreadsKeybindings (\s x -> s { _ivSearchThreadsKeybindings = x })

ivManageMailTagsKeybindings :: Lens' IndexViewSettings [Keybinding 'Mails 'ManageMailTagsEditor]
ivManageMailTagsKeybindings = lens _ivManageMailTagsKeybindings (\s x -> s { _ivManageMailTagsKeybindings = x })

ivManageThreadTagsKeybindings :: Lens' IndexViewSettings [Keybinding 'Threads 'ManageThreadTagsEditor]
ivManageThreadTagsKeybindings = lens _ivManageThreadTagsKeybindings (\s x -> s { _ivManageThreadTagsKeybindings = x })

ivFromKeybindings :: Lens' IndexViewSettings [Keybinding 'Threads 'ComposeFrom]
ivFromKeybindings = lens _ivFromKeybindings (\s x -> s { _ivFromKeybindings = x })

ivToKeybindings :: Lens' IndexViewSettings [Keybinding 'Threads 'ComposeTo]
ivToKeybindings = lens _ivToKeybindings (\s x -> s { _ivToKeybindings = x })

ivSubjectKeybindings :: Lens' IndexViewSettings [Keybinding 'Threads 'ComposeSubject]
ivSubjectKeybindings = lens _ivSubjectKeybindings (\s x -> s { _ivSubjectKeybindings = x })


data MailViewSettings = MailViewSettings
    { _mvIndexRows           :: Int
    , _mvPreferredContentType :: ContentType
    , _mvHeadersToShow       :: CI.CI B.ByteString -> Bool
    , _mvKeybindings         :: [Keybinding 'ViewMail 'ScrollingMailView]
    , _mvManageMailTagsKeybindings :: [Keybinding 'ViewMail 'ManageMailTagsEditor]
    }

mvIndexRows :: Lens' MailViewSettings Int
mvIndexRows = lens _mvIndexRows (\mv x -> mv { _mvIndexRows = x })

mvPreferredContentType :: Lens' MailViewSettings ContentType
mvPreferredContentType = lens _mvPreferredContentType (\mv x -> mv { _mvPreferredContentType = x })

mvHeadersToShow :: Getter MailViewSettings (CI.CI B.ByteString -> Bool)
mvHeadersToShow = lens _mvHeadersToShow (\mv x -> mv { _mvHeadersToShow = x })

mvKeybindings :: Lens' MailViewSettings [Keybinding 'ViewMail 'ScrollingMailView]
mvKeybindings = lens _mvKeybindings (\mv x -> mv { _mvKeybindings = x })

mvManageMailTagsKeybindings :: Lens' MailViewSettings [Keybinding 'ViewMail 'ManageMailTagsEditor]
mvManageMailTagsKeybindings = lens _mvManageMailTagsKeybindings (\mv x -> mv { _mvManageMailTagsKeybindings = x })

data ViewName
    = Threads
    | Mails
    | ViewMail
    | ComposeView
    | Help
    | FileBrowser
    deriving (Eq,Ord,Show)

data View = View
    { _vFocus :: Brick.FocusRing Name
    , _vWidgets :: [Name]
    }

vWidgets :: Lens' View [Name]
vWidgets = lens _vWidgets (\settings x -> settings { _vWidgets = x })

vFocus :: Lens' View (Brick.FocusRing Name)
vFocus = lens _vFocus (\settings x -> settings { _vFocus = x})

data ViewSettings = ViewSettings
    { _vsViews :: Map.Map ViewName View
    , _vsFocusedView :: Brick.FocusRing ViewName
    }

vsViews :: Lens' ViewSettings (Map.Map ViewName View)
vsViews = lens _vsViews (\settings x -> settings { _vsViews = x })

vsFocusedView :: Lens' ViewSettings (Brick.FocusRing ViewName)
vsFocusedView = lens _vsFocusedView (\settings x -> settings { _vsFocusedView = x})

data FileSystemEntry
    = Directory String
    | File String
    deriving (Show,Ord,Eq)

fsEntryName :: Getter FileSystemEntry String
fsEntryName = let toName (Directory n) = n
                  toName (File n) = n
              in to toName

data FileBrowser = CreateFileBrowser
  { _fbEntries :: L.List Name (Bool, FileSystemEntry)
  , _fbSearchPath :: E.Editor FilePath Name
  }

fbEntries :: Lens' FileBrowser (L.List Name (Bool, FileSystemEntry))
fbEntries = lens _fbEntries (\cv x -> cv { _fbEntries = x })

fbSearchPath :: Lens' FileBrowser (E.Editor FilePath Name)
fbSearchPath = lens _fbSearchPath (\c x -> c { _fbSearchPath = x})

-- | Overall application state
data AppState = AppState
    { _asConfig :: InternalConfiguration
    , _asMailIndex :: MailIndex
    , _asMailView  :: MailView
    , _asCompose   :: Compose  -- ^ state to keep when user creates a new mail
    , _asError     :: Maybe Error -- ^ in case of errors, show this error message
    , _asViews     :: ViewSettings -- ^ stores widget and focus information
    , _asFileBrowser :: FileBrowser
    }

asConfig :: Lens' AppState InternalConfiguration
asConfig = lens _asConfig (\appstate x -> appstate { _asConfig = x })

asMailIndex :: Lens' AppState MailIndex
asMailIndex = lens _asMailIndex (\appstate x -> appstate { _asMailIndex = x })

asMailView :: Lens' AppState MailView
asMailView = lens _asMailView (\appstate x -> appstate { _asMailView = x })

asCompose :: Lens' AppState Compose
asCompose = lens _asCompose (\appstate x -> appstate { _asCompose = x })

asError :: Lens' AppState (Maybe Error)
asError = lens _asError (\appstate x -> appstate { _asError = x })

asViews :: Lens' AppState ViewSettings
asViews f (AppState a b c d e g h) = fmap (\g' -> AppState a b c d e g' h) (f g)

asFileBrowser :: Lens' AppState FileBrowser
asFileBrowser = lens _asFileBrowser (\as x -> as { _asFileBrowser = x })

data Action (v :: ViewName) (ctx :: Name) a = Action
    { _aDescription :: [T.Text]
    -- ^ sequential list of things that the action does
    , _aAction :: AppState -> EventM Name a
    }

aAction :: Getter (Action v ctx a) (AppState -> EventM Name a)
aAction = to (\(Action _ b) -> b)

data Keybinding (v :: ViewName) (ctx :: Name) = Keybinding
    { _kbEvent :: Vty.Event
    , _kbAction :: Action v ctx (Next AppState)
    }
instance Eq (Keybinding v ctx) where
  (==) (Keybinding a _) (Keybinding b _) = a == b
  (/=) (Keybinding a _) (Keybinding b _) = a /= b

kbEvent :: Getter (Keybinding v ctx) Vty.Event
kbEvent = to (\(Keybinding b _) -> b)

kbAction :: Getter (Keybinding v ctx) (Action v ctx (Next AppState))
kbAction = to (\(Keybinding _ c) -> c)

aDescription :: Getter (Action v ctx a) [T.Text]
aDescription = to (\(Action a _ ) -> a)

-- | an email from the notmuch database
data NotmuchMail = NotmuchMail
    { _mailSubject :: T.Text
    , _mailFrom :: T.Text
    , _mailDate :: UTCTime
    , _mailTags :: [Tag]
    , _mailId :: B.ByteString
    } deriving (Show, Eq)

mailSubject :: Lens' NotmuchMail T.Text
mailSubject = lens _mailSubject (\m s -> m { _mailSubject = s })

mailFrom :: Lens' NotmuchMail T.Text
mailFrom = lens _mailFrom (\m f -> m { _mailFrom = f })

mailDate :: Lens' NotmuchMail UTCTime
mailDate = lens _mailDate (\m d -> m { _mailDate = d })

mailTags :: Lens' NotmuchMail [Tag]
mailTags = lens _mailTags (\m t -> m { _mailTags = t })

mailId :: Lens' NotmuchMail B.ByteString
mailId = lens _mailId (\m i -> m { _mailId = i })

data NotmuchThread = NotmuchThread
    { _thSubject :: T.Text
    , _thAuthors :: [T.Text]
    , _thDate :: UTCTime
    , _thTags :: [Tag]
    , _thReplies :: Int
    , _thId :: B.ByteString
    } deriving (Show, Eq)

thSubject :: Lens' NotmuchThread T.Text
thSubject = lens _thSubject (\m s -> m { _thSubject = s })

thAuthors :: Lens' NotmuchThread [T.Text]
thAuthors = lens _thAuthors (\m f -> m { _thAuthors = f })

thDate :: Lens' NotmuchThread UTCTime
thDate = lens _thDate (\m d -> m { _thDate = d })

thTags :: Lens' NotmuchThread [Tag]
thTags = lens _thTags (\m t -> m { _thTags = t })

thReplies :: Lens' NotmuchThread Int
thReplies = lens _thReplies (\m t -> m { _thReplies = t })

thId :: Lens' NotmuchThread B.ByteString
thId = lens _thId (\m t -> m { _thId = t })

-- | Utility for safe conversion from bytestring to text
decodeLenient :: B.ByteString -> T.Text
decodeLenient = T.decodeUtf8With T.lenientDecode

-- | Tag operations
data TagOp = RemoveTag Tag | AddTag Tag | ResetTags
  deriving (Show, Eq)
