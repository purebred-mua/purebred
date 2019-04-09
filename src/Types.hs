{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | Basic types for the UI used by this library
module Types
  ( module Types
  , Tag
  , module Purebred.Events
  ) where

import GHC.Generics (Generic)

import Brick.BChan (BChan)
import qualified Brick.Focus as Brick
import Brick.Themes (Theme)
import Brick.Types (EventM, Next)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Control.DeepSeq (NFData(rnf), force)
import Control.Lens
import qualified Data.Map as Map
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Vector as V
import qualified Graphics.Vty.Input.Events as Vty
import Data.Time (UTCTime)
import qualified Data.CaseInsensitive as CI
import System.Process.Typed (ProcessConfig)

import Notmuch (Tag)
import Data.MIME

import Error
import Purebred.Events
import Purebred.LazyVector (V)

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}

-- | Used to identify widgets in brick
data Name =
    SearchThreadsEditor
    | ListOfMails
    | ListOfThreads
    | ScrollingMailView
    | ComposeHeaders
    | ComposeFrom
    | ComposeTo
    | ComposeSubject
    | ComposeListOfAttachments
    | ScrollingHelpView
    | ManageMailTagsEditor
    | ManageThreadTagsEditor
    | ListOfFiles
    | ManageFileBrowserSearchPath
    | MailListOfAttachments
    | MailAttachmentOpenWithEditor
    | MailAttachmentPipeToEditor
    | StatusBar
    deriving (Eq,Show,Ord)

-- | A brick list, with a field that optionally contains its length.
--
-- Rather than reading the length from the underlying list, to support
-- lazy loading we have a separate field that optionally contains the
-- length.  Widgets should read the length from this field and must
-- handle the @Nothing@ case.
--
-- For strict lists (e.g. Vector-based) the length can be recorded when
-- constructed.  For lazy lists, it could be left empty, or a thread
-- could be spawned to compute the length in the background and update
-- the value when the length is known.
--
data ListWithLength t a = ListWithLength (L.GenericList Name t a) (Maybe Int)

listList :: Lens' (ListWithLength t a) (L.GenericList Name t a)
listList f (ListWithLength a b) = (\a' -> ListWithLength a' b) <$> f a

listLength :: Lens' (ListWithLength t a) (Maybe Int)
listLength f (ListWithLength a b) = (\b' -> ListWithLength a b') <$> f b


{- | main application interface

The main UI shows a list of e-mails, allows the user to manipulate the notmuch
search and composes e-mails from here.

-}
data MailIndex = MailIndex
    { _miListOfMails  :: ListWithLength V.Vector NotmuchMail
    , _miListOfThreads :: ListWithLength V NotmuchThread
    , _miListOfThreadsGeneration :: Generation
    , _miSearchThreadsEditor :: E.Editor T.Text Name
    , _miMailTagsEditor :: E.Editor T.Text Name
    , _miThreadTagsEditor :: E.Editor T.Text Name
    }

miMails :: Lens' MailIndex (ListWithLength V.Vector NotmuchMail)
miMails = lens _miListOfMails (\m v -> m { _miListOfMails = v })

miThreads :: Lens' MailIndex (ListWithLength V NotmuchThread)
miThreads = lens _miListOfThreads (\m v -> m { _miListOfThreads = v})

miListOfMails :: Lens' MailIndex (L.GenericList Name V.Vector NotmuchMail)
miListOfMails = miMails . listList

miListOfThreads :: Lens' MailIndex (L.GenericList Name V NotmuchThread)
miListOfThreads = miThreads . listList

miListOfThreadsGeneration :: Lens' MailIndex Generation
miListOfThreadsGeneration =
  lens _miListOfThreadsGeneration (\s b -> s { _miListOfThreadsGeneration = b })

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
    , _mvAttachments :: L.List Name WireEntity
    , _mvOpenCommand:: E.Editor T.Text Name
    , _mvPipeCommand :: E.Editor T.Text Name
    }

mvMail :: Lens' MailView (Maybe MIMEMessage)
mvMail = lens _mvMail (\mv pm -> mv { _mvMail = pm })

mvHeadersState :: Lens' MailView HeadersState
mvHeadersState = lens _mvHeadersState (\mv hs -> mv { _mvHeadersState = hs })

mvAttachments :: Lens' MailView (L.List Name WireEntity)
mvAttachments = lens _mvAttachments (\mv hs -> mv { _mvAttachments = hs })

mvOpenCommand :: Lens' MailView (E.Editor T.Text Name)
mvOpenCommand = lens _mvOpenCommand (\mv hs -> mv { _mvOpenCommand = hs })

mvPipeCommand :: Lens' MailView (E.Editor T.Text Name)
mvPipeCommand = lens _mvPipeCommand (\mv hs -> mv { _mvPipeCommand = hs })

data Compose = Compose
    { _cMail :: B.ByteString
    , _cFrom :: E.Editor T.Text Name
    , _cTo :: E.Editor T.Text Name
    , _cSubject :: E.Editor T.Text Name
    , _cTemp :: T.Text
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

cTemp :: Lens' Compose T.Text
cTemp = lens _cTemp (\c x -> c { _cTemp = x })

cAttachments :: Lens' Compose (L.List Name MIMEMessage)
cAttachments = lens _cAttachments (\c x -> c { _cAttachments = x })

data NotmuchSettings a = NotmuchSettings
    { _nmSearch :: T.Text
    , _nmDatabase :: a
    , _nmNewTag :: Tag
    }
    deriving (Generic, NFData)

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
  deriving (Generic, NFData)

fbKeybindings :: Lens' (FileBrowserSettings a) [Keybinding 'FileBrowser 'ListOfFiles]
fbKeybindings = lens _fbKeybindings (\cv x -> cv { _fbKeybindings = x })

fbSearchPathKeybindings :: Lens' (FileBrowserSettings a) [Keybinding 'FileBrowser 'ManageFileBrowserSearchPath]
fbSearchPathKeybindings = lens _fbSearchPathKeybindings (\cv x -> cv { _fbSearchPathKeybindings = x})

fbHomePath :: Lens (FileBrowserSettings a) (FileBrowserSettings a') a a'
fbHomePath = lens _fbHomePath (\s a -> s { _fbHomePath = a })

data Configuration extra a b c = Configuration
    { _confTheme :: Theme
    , _confNotmuch :: NotmuchSettings a
    , _confEditor :: b
    , _confMailView :: MailViewSettings
    , _confIndexView :: IndexViewSettings
    , _confComposeView :: ComposeViewSettings
    , _confHelpView :: HelpViewSettings
    , _confDefaultView :: ViewName
    , _confFileBrowserView :: FileBrowserSettings c
    , _confExtra :: extra  -- data specific to a particular "phase" of configuration
    }
    deriving (Generic, NFData)

type UserConfiguration = Configuration () (IO FilePath) (IO String) (IO FilePath)
type InternalConfiguration = Configuration (BChan PurebredEvent, String) FilePath String FilePath

type ConfigurationLens v = forall z a b c. Lens' (Configuration z a b c) v

confTheme :: ConfigurationLens Theme
confTheme = lens _confTheme (\c x -> c { _confTheme = x })

confEditor :: Lens (Configuration z a b c) (Configuration z a b' c) b b'
confEditor = lens _confEditor (\conf x -> conf { _confEditor = x })

confNotmuch :: Lens (Configuration z a b c) (Configuration z a' b c) (NotmuchSettings a) (NotmuchSettings a')
confNotmuch = lens _confNotmuch (\conf x -> conf { _confNotmuch = x })

confMailView :: ConfigurationLens MailViewSettings
confMailView = lens _confMailView (\conf x -> conf { _confMailView = x })

confIndexView :: ConfigurationLens IndexViewSettings
confIndexView = lens _confIndexView (\conf x -> conf { _confIndexView = x })

confComposeView :: ConfigurationLens ComposeViewSettings
confComposeView = lens _confComposeView (\conf x -> conf { _confComposeView = x})

confHelpView :: ConfigurationLens HelpViewSettings
confHelpView = lens _confHelpView (\conf x -> conf { _confHelpView = x })

confDefaultView :: ConfigurationLens ViewName
confDefaultView = lens _confDefaultView (\conf x -> conf { _confDefaultView = x })

confFileBrowserView :: Lens (Configuration z a b c) (Configuration z a b c') (FileBrowserSettings c) (FileBrowserSettings c')
confFileBrowserView = lens _confFileBrowserView (\conf x -> conf { _confFileBrowserView = x })

confExtra :: Lens (Configuration extra a b c) (Configuration extra' a b c) extra extra'
confExtra = lens _confExtra (\cfg x -> cfg { _confExtra = x })

confBChan :: Lens' InternalConfiguration (BChan PurebredEvent)
confBChan = confExtra . _1

confBoundary :: Lens' InternalConfiguration String
confBoundary = confExtra . _2


data ComposeViewSettings = ComposeViewSettings
    { _cvFromKeybindings :: [Keybinding 'ComposeView 'ComposeFrom]
    , _cvToKeybindings :: [Keybinding 'ComposeView 'ComposeTo]
    , _cvSubjectKeybindings :: [Keybinding 'ComposeView 'ComposeSubject]
    , _cvSendMailCmd :: B.ByteString -> IO (Either Error ())
    , _cvListOfAttachmentsKeybindings :: [Keybinding 'ComposeView 'ComposeListOfAttachments]
    , _cvIdentities :: [Mailbox]
    }
    deriving (Generic, NFData)

cvFromKeybindings :: Lens' ComposeViewSettings [Keybinding 'ComposeView 'ComposeFrom]
cvFromKeybindings = lens _cvFromKeybindings (\cv x -> cv { _cvFromKeybindings = x })

cvToKeybindings :: Lens' ComposeViewSettings [Keybinding 'ComposeView 'ComposeTo]
cvToKeybindings = lens _cvToKeybindings (\cv x -> cv { _cvToKeybindings = x })

cvSubjectKeybindings :: Lens' ComposeViewSettings [Keybinding 'ComposeView 'ComposeSubject]
cvSubjectKeybindings = lens _cvSubjectKeybindings (\cv x -> cv { _cvSubjectKeybindings = x })

cvSendMailCmd :: Lens' ComposeViewSettings (B.ByteString -> IO (Either Error ()))
cvSendMailCmd = lens _cvSendMailCmd (\cv x -> cv { _cvSendMailCmd = x })

cvListOfAttachmentsKeybindings :: Lens' ComposeViewSettings [Keybinding 'ComposeView 'ComposeListOfAttachments]
cvListOfAttachmentsKeybindings = lens _cvListOfAttachmentsKeybindings (\cv x -> cv { _cvListOfAttachmentsKeybindings = x })

cvIdentities :: Lens' ComposeViewSettings [Mailbox]
cvIdentities = lens _cvIdentities (\cv x -> cv { _cvIdentities = x })

newtype HelpViewSettings = HelpViewSettings
  { _hvKeybindings :: [Keybinding 'Help 'ScrollingHelpView]
  }
  deriving (Generic, NFData)

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
    deriving (Generic, NFData)

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
    , _mvMailListOfAttachmentsKeybindings :: [Keybinding 'ViewMail 'MailListOfAttachments]
    , _mvOpenWithKeybindings :: [Keybinding 'ViewMail 'MailAttachmentOpenWithEditor]
    , _mvPipeToKeybindings :: [Keybinding 'ViewMail 'MailAttachmentPipeToEditor]
    , _mvMailcap :: [(ContentType -> Bool, String)]
    }
    deriving (Generic, NFData)

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

mvMailListOfAttachmentsKeybindings :: Lens' MailViewSettings [Keybinding 'ViewMail 'MailListOfAttachments]
mvMailListOfAttachmentsKeybindings = lens _mvMailListOfAttachmentsKeybindings (\s x -> s { _mvMailListOfAttachmentsKeybindings = x })

mvOpenWithKeybindings :: Lens' MailViewSettings [Keybinding 'ViewMail 'MailAttachmentOpenWithEditor]
mvOpenWithKeybindings = lens _mvOpenWithKeybindings (\s x -> s { _mvOpenWithKeybindings = x })

mvPipeToKeybindings :: Lens' MailViewSettings [Keybinding 'ViewMail 'MailAttachmentPipeToEditor]
mvPipeToKeybindings = lens _mvPipeToKeybindings (\s x -> s { _mvPipeToKeybindings = x })

mvMailcap :: Lens' MailViewSettings [(ContentType -> Bool, String)]
mvMailcap = lens _mvMailcap (\s x -> s { _mvMailcap = x })

data ViewName
    = Threads
    | Mails
    | ViewMail
    | ComposeView
    | Help
    | FileBrowser
    deriving (Eq, Ord, Show, Generic, NFData)

data ViewState
  = Hidden
  | Visible
  deriving (Show, Eq)

-- A view element is a name for a widget with a given view state
data Tile =
  Tile ViewState
       Name
  deriving (Show, Eq)

veName :: Lens' Tile Name
veName f (Tile a b) = fmap (\b' -> Tile a b') (f b)

veState :: Lens' Tile ViewState
veState f (Tile a b) = fmap (\a' -> Tile a' b) (f a)

data View = View
    { _vFocus :: Name
    , _vWidgets :: Tiles
    }

-- A tile is a view element with a widget and it's view state
newtype Tiles =
  Tiles (V.Vector Tile)
  deriving (Eq, Show)

type instance Index Tiles = Name
type instance IxValue Tiles = Tile

instance Ixed Tiles where
  ix = tile

tileiso :: Iso' Tiles (V.Vector Tile)
tileiso = iso (\(Tiles xs) -> xs) Tiles

tile :: Name -> Traversal' Tiles Tile
tile k = tileiso . traversed . filtered (\x -> k == view veName x)

vWidgets :: Lens' View Tiles
vWidgets = lens _vWidgets (\settings x -> settings { _vWidgets = x })

vFocus :: Lens' View Name
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
    , _asLocalTime :: UTCTime
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
asViews f (AppState a b c d e g h i) = fmap (\g' -> AppState a b c d e g' h i) (f g)

asFileBrowser :: Lens' AppState FileBrowser
asFileBrowser = lens _asFileBrowser (\as x -> as { _asFileBrowser = x })

asLocalTime :: Lens' AppState UTCTime
asLocalTime = lens _asLocalTime (\as x -> as { _asLocalTime = x })

data Action (v :: ViewName) (ctx :: Name) a = Action
    { _aDescription :: [T.Text]
    -- ^ sequential list of things that the action does
    , _aAction :: AppState -> EventM Name a
    }
    deriving (Generic, NFData)

aAction :: Getter (Action v ctx a) (AppState -> EventM Name a)
aAction = to (\(Action _ b) -> b)

data Keybinding (v :: ViewName) (ctx :: Name) = Keybinding
    { _kbEvent :: Vty.Event
    , _kbAction :: Action v ctx (Next AppState)
    }

-- | __HACK__: the 'Vty.Event' is only evaluated to WHNF.
-- There is no 'NFData' instance for 'Vty.Event' and I don't want
-- to make an orphan instance for it.
instance NFData (Keybinding v ctx) where
  rnf (Keybinding ev act) = Keybinding ev (force act) `seq` ()

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

-- | A bracket-style type for creating and releasing acquired resources (e.g.
-- temporary files). Note that extending this is perhaps not worth it and we
-- should perhaps look at ResourceT if necessary.
data ResourceSpec a = ResourceSpec
 { _rsAcquire :: IO a
 -- ^ acquire a resource (e.g. create temporary file)
 , _rsFree :: a -> IO ()
 -- ^ release a resource (e.g. remove temporary file)
 , _rsUpdate :: a -> B.ByteString -> IO ()
 -- ^ update the acquired resource with the ByteString obtained from serialising the WireEntity
 }

rsAcquire :: Lens' (ResourceSpec a) (IO a)
rsAcquire = lens _rsAcquire (\rs x -> rs { _rsAcquire = x })

rsFree :: Lens' (ResourceSpec a) (a -> IO ())
rsFree = lens _rsFree (\rs x -> rs { _rsFree = x })

rsUpdate :: Lens' (ResourceSpec a) (a -> B.ByteString -> IO ())
rsUpdate = lens _rsUpdate (\rs x -> rs { _rsUpdate = x })

-- | Command configuration which is bound to an acquired resource (e.g. a
-- tempfile) which may or may not be cleaned up after exit of it's process.
data EntityCommand a = EntityCommand
  { _ccAfterExit :: AppState -> a -> IO AppState
  , _ccResource :: ResourceSpec a
  , _ccProcessConfig :: B.ByteString -> a -> ProcessConfig () () ()
  , _ccEntity :: B.ByteString
  -- ^ The decoded Entity
  }

ccAfterExit :: Lens' (EntityCommand a) (AppState -> a -> IO AppState)
ccAfterExit = lens _ccAfterExit (\cc x -> cc { _ccAfterExit = x })

ccEntity :: Lens' (EntityCommand a) B.ByteString
ccEntity = lens _ccEntity (\cc x -> cc { _ccEntity = x })

ccProcessConfig :: Lens' (EntityCommand a) (B.ByteString -> a -> ProcessConfig () () ())
ccProcessConfig = lens _ccProcessConfig (\cc x -> cc { _ccProcessConfig = x })

ccResource :: Lens' (EntityCommand a) (ResourceSpec a)
ccResource = lens _ccResource (\cc x -> cc { _ccResource = x })
