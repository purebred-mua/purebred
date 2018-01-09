{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

-- | Basic types for the UI used by this library
module Types
  ( module Types
  , Tag
  ) where

import qualified Brick.AttrMap as Brick
import qualified Brick.Focus as Brick
import Brick.Types (EventM, Next)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Control.Lens
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
    | AddAttachment  -- ^ specify file path to include in mail
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
    | BrowseFilesEditor
    | ListOfFiles
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
    { _mvMail :: Maybe (Message MIME)
    , _mvHeadersState :: HeadersState
    }

mvMail :: Lens' MailView (Maybe (Message MIME))
mvMail = lens _mvMail (\mv pm -> mv { _mvMail = pm })

mvHeadersState :: Lens' MailView HeadersState
mvHeadersState = lens _mvHeadersState (\mv hs -> mv { _mvHeadersState = hs })

data Compose = Compose
    { _cTmpFile :: Maybe String
    , _cFrom    :: E.Editor T.Text Name
    , _cTo      :: E.Editor T.Text Name
    , _cSubject :: E.Editor T.Text Name
    , _cFocusFields :: Brick.FocusRing Name
    }

cTmpFile :: Lens' Compose (Maybe String)
cTmpFile = lens _cTmpFile (\c x -> c { _cTmpFile = x })

cFrom :: Lens' Compose (E.Editor T.Text Name)
cFrom = lens _cFrom (\c x -> c { _cFrom = x })

cTo :: Lens' Compose (E.Editor T.Text Name)
cTo = lens _cTo (\c x -> c { _cTo = x })

cSubject :: Lens' Compose (E.Editor T.Text Name)
cSubject = lens _cSubject (\c x -> c { _cSubject = x })

cFocusFields :: Lens' Compose (Brick.FocusRing Name)
cFocusFields = lens _cFocusFields (\cv x -> cv { _cFocusFields = x })

cFocusedEditorL
    :: Functor f
    => Name
    -> (E.Editor T.Text Name -> f (E.Editor T.Text Name))
    -> AppState
    -> f AppState
cFocusedEditorL ComposeTo = asCompose . cTo
cFocusedEditorL ComposeFrom = asCompose . cFrom
cFocusedEditorL _ = asCompose . cSubject

data NotmuchSettings a = NotmuchSettings
    { _nmSearch :: T.Text
    , _nmDatabase :: a
    , _nmNewTag :: Tag
    }

nmSearch :: Lens' (NotmuchSettings a) T.Text
nmSearch f (NotmuchSettings a b c) = fmap (\a' -> NotmuchSettings a' b c) (f a)

nmDatabase :: Lens (NotmuchSettings a) (NotmuchSettings b) a b
nmDatabase f (NotmuchSettings a b c) = fmap (\b' -> NotmuchSettings a b' c) (f b)

nmNewTag :: Getter (NotmuchSettings a) Tag
nmNewTag = to (\(NotmuchSettings _ _ c) -> c)


data BrowseFilesSettings a = BrowseFilesSettings
  { _bfKeybindings :: [Keybinding 'AddAttachment (Next AppState)]
  , _bfHomePath :: a
  }

bfKeybindings :: Lens' (BrowseFilesSettings a) [Keybinding 'AddAttachment (Next AppState)]
bfKeybindings = lens _bfKeybindings (\cv x -> cv { _bfKeybindings = x })

bfHomePath :: Lens (BrowseFilesSettings a) (BrowseFilesSettings a') a a'
bfHomePath = lens _bfHomePath (\s a -> s { _bfHomePath = a })

data Configuration a b c = Configuration
    { _confColorMap :: Brick.AttrMap
    , _confNotmuch :: NotmuchSettings a
    , _confEditor :: b
    , _confMailView :: MailViewSettings
    , _confIndexView :: IndexViewSettings
    , _confComposeView :: ComposeViewSettings
    , _confHelpView :: HelpViewSettings
    , _confBrowseFilesView :: BrowseFilesSettings c
    }

type UserConfiguration = Configuration (IO FilePath) (IO String) (IO FilePath)
type InternalConfiguration = Configuration FilePath String FilePath

-- | Shorthand for optics to concrete fields
type ConfigurationLens v = forall a b c. Lens' (Configuration a b c) v

confColorMap :: Getter (Configuration a b c) Brick.AttrMap
confColorMap = to (\(Configuration a _ _ _ _ _ _ _) -> a)

confEditor :: Lens (Configuration a b c) (Configuration a b' c) b b'
confEditor = lens _confEditor (\conf x -> conf { _confEditor = x })

confNotmuch :: Lens (Configuration a b c) (Configuration a' b c) (NotmuchSettings a) (NotmuchSettings a')
confNotmuch = lens _confNotmuch (\conf x -> conf { _confNotmuch = x })

confMailView :: ConfigurationLens MailViewSettings
confMailView = lens _confMailView (\conf x -> conf { _confMailView = x })

confIndexView :: ConfigurationLens IndexViewSettings
confIndexView = lens _confIndexView (\conf x -> conf { _confIndexView = x })

confComposeView :: ConfigurationLens ComposeViewSettings
confComposeView = lens _confComposeView (\conf x -> conf { _confComposeView = x })

confHelpView :: ConfigurationLens HelpViewSettings
confHelpView = lens _confHelpView (\conf x -> conf { _confHelpView = x })

confBrowseFilesView :: Lens (Configuration a b c) (Configuration a b c') (BrowseFilesSettings c) (BrowseFilesSettings c')
confBrowseFilesView = lens _confBrowseFilesView (\conf x -> conf { _confBrowseFilesView = x })

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
    , _mvPreferedContentType :: ContentType
    , _mvHeadersToShow       :: CI.CI B.ByteString -> Bool
    , _mvKeybindings         :: [Keybinding 'ViewMail (Next AppState)]
    , _mvIndexKeybindings    :: [Keybinding 'BrowseMail (Next AppState)]
    }

mvIndexRows :: Lens' MailViewSettings Int
mvIndexRows f (MailViewSettings a b c d e) = fmap (\a' -> MailViewSettings a' b c d e) (f a)

mvPreferredContentType :: Lens' MailViewSettings ContentType
mvPreferredContentType f (MailViewSettings a b c d e) = fmap (\b' -> MailViewSettings a b' c d e) (f b)

mvHeadersToShow :: Getter MailViewSettings (CI.CI B.ByteString -> Bool)
mvHeadersToShow = to (\(MailViewSettings _ _ h _ _) -> h)

mvKeybindings :: Lens' MailViewSettings [Keybinding 'ViewMail (Next AppState)]
mvKeybindings f (MailViewSettings a b c d e) = fmap (\d' -> MailViewSettings a b c d' e) (f d)

mvIndexKeybindings :: Lens' MailViewSettings [Keybinding 'BrowseMail (Next AppState)]
mvIndexKeybindings f (MailViewSettings a b c d e) = fmap (\e' -> MailViewSettings a b c d e') (f e)

data FileSystemEntry
    = Directory String
    | File String
    deriving (Show)

fsEntryName :: Getter FileSystemEntry String
fsEntryName = let toName (Directory n) = n
                  toName (File n) = n
              in to toName

data BrowseFiles = BrowseFiles
  { _bfEntries :: L.List Name (Bool, FileSystemEntry)
  , _bfSearchPath :: FilePath
  }

bfEntries :: Lens' BrowseFiles (L.List Name (Bool, FileSystemEntry))
bfEntries = lens _bfEntries (\cv x -> cv { _bfEntries = x })

bfSearchPath :: Lens' BrowseFiles FilePath
bfSearchPath = lens _bfSearchPath (\c x -> c { _bfSearchPath = x})

-- | Overall application state
data AppState = AppState
    { _asConfig :: InternalConfiguration
    , _asMailIndex :: MailIndex
    , _asMailView :: MailView
    , _asCompose :: Compose  -- ^ state to keep when user creates a new mail
    , _asAppMode :: Mode
    , _asError :: Maybe Error -- ^ in case of errors, show this error message
    , _asBrowseFiles :: BrowseFiles
    }

asConfig :: Lens' AppState InternalConfiguration
asConfig = lens _asConfig (\as x -> as { _asConfig = x })

asMailIndex :: Lens' AppState MailIndex
asMailIndex = lens _asMailIndex (\as x -> as { _asMailIndex = x })

asMailView :: Lens' AppState MailView
asMailView  = lens _asMailView (\as x -> as { _asMailView = x })

asCompose :: Lens' AppState Compose
asCompose = lens _asCompose (\as x -> as { _asCompose = x })

asAppMode :: Lens' AppState Mode
asAppMode = lens _asAppMode (\as x -> as { _asAppMode = x })

asError :: Lens' AppState (Maybe Error)
asError = lens _asError (\as x -> as { _asError = x })

asBrowseFiles :: Lens' AppState BrowseFiles
asBrowseFiles = lens _asBrowseFiles (\as x -> as { _asBrowseFiles = x })

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
