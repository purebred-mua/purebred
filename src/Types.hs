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
    = BrowseMail   -- ^ input focus goes to navigating the list of mails (main screen)
    | SearchMail   -- ^ input focus goes to manipulating the notmuch search (main screen)
    | ViewMail   -- ^ focus is on the screen showing the entire mail
    | GatherHeaders   -- ^ focus is on the command line to gather input for composing an e-mail
    | ComposeEditor   -- ^ edit the final e-mail
    | Help  -- ^ shows all keybindings
    | ManageTags -- ^ add/remove tags
    deriving (Eq,Show,Ord)

-- | Used to identify widgets in brick
data Name =
    EditorInput
    | ListOfMails
    | ScrollingMailView
    | GatherHeadersFrom
    | GatherHeadersTo
    | GatherHeadersSubject
    | ScrollingHelpView
    | ManageTagsEditor
    deriving (Eq,Show,Ord)

{- | main application interface

The main UI shows a list of e-mails, allows the user to manipulate the notmuch
search and composes e-mails from here.

-}
data MailIndex = MailIndex
    { _miListOfMails  :: L.List Name NotmuchMail
    , _miSearchEditor :: E.Editor T.Text Name
    }

miListOfMails :: Lens' MailIndex (L.List Name NotmuchMail)
miListOfMails f (MailIndex a b) = fmap (\a' -> MailIndex a' b) (f a)

miSearchEditor :: Lens' MailIndex (E.Editor T.Text Name)
miSearchEditor f (MailIndex a b) = fmap (\b' -> MailIndex a b') (f b)

data HeadersState = ShowAll | Filtered

data MailView = MailView
    { _mvMail :: Maybe ParsedMail
    , _mvHeadersState :: HeadersState
    }

mvMail :: Lens' MailView (Maybe ParsedMail)
mvMail = lens _mvMail (\mv pm -> mv { _mvMail = pm })

mvHeadersState :: Lens' MailView HeadersState
mvHeadersState = lens _mvHeadersState (\mv hs -> mv { _mvHeadersState = hs })

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

cTmpFile :: Lens' Compose (Maybe String)
cTmpFile f (Compose a b c d e) = fmap (\a' -> Compose a' b c d e) (f a)

cFocus :: Lens' Compose ComposeState
cFocus f (Compose a b c d e) = fmap (\b' -> Compose a b' c d e) (f b)

cFrom :: Lens' Compose (E.Editor T.Text Name)
cFrom f (Compose a b c d e) = fmap (\c' -> Compose a b c' d e) (f c)

cTo :: Lens' Compose (E.Editor T.Text Name)
cTo f (Compose a b c d e) = fmap (\d' -> Compose a b c d' e) (f d)

cSubject :: Lens' Compose (E.Editor T.Text Name)
cSubject f (Compose a b c d e) = fmap (\e' -> Compose a b c d e') (f e)


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

confComposeView :: Getter (Configuration a b) ComposeViewSettings
confComposeView = to (\(Configuration _ _ _ _ _ g _) -> g)

confHelpView :: Getter (Configuration a b) HelpViewSettings
confHelpView = to (\(Configuration _ _ _ _ _ _ h) -> h)

newtype ComposeViewSettings = ComposeViewSettings
    { _cvKeybindings :: [Keybinding 'ComposeEditor (Next AppState)]
    }

cvKeybindings :: Lens' ComposeViewSettings [Keybinding 'ComposeEditor (Next AppState)]
cvKeybindings f (ComposeViewSettings a) = fmap (\a' -> ComposeViewSettings a') (f a)

newtype HelpViewSettings = HelpViewSettings
  { _hvKeybindings :: [Keybinding 'Help (Next AppState)]
  }

hvKeybindings :: Lens' HelpViewSettings [Keybinding 'Help (Next AppState)]
hvKeybindings f (HelpViewSettings a) = fmap (\a' -> HelpViewSettings a') (f a)

data IndexViewSettings = IndexViewSettings
    { _ivKeybindings :: [Keybinding 'BrowseMail (Next AppState)]
    , _ivSearchKeybindings :: [Keybinding 'SearchMail (Next AppState)]
    , _ivManageTagsKeybindings :: [Keybinding 'ManageTags (Next AppState)]
    }

ivKeybindings :: Lens' IndexViewSettings [Keybinding 'BrowseMail (Next AppState)]
ivKeybindings f (IndexViewSettings a b c) = fmap (\a' -> IndexViewSettings a' b c) (f a)

ivSearchKeybindings :: Lens' IndexViewSettings [Keybinding 'SearchMail (Next AppState)]
ivSearchKeybindings f (IndexViewSettings a b c) = fmap (\b' -> IndexViewSettings a b' c) (f b)

ivManageTagsKeybindings :: Lens' IndexViewSettings [Keybinding 'ManageTags (Next AppState)]
ivManageTagsKeybindings f (IndexViewSettings a b c) = fmap (\c' -> IndexViewSettings a b c') (f c)

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

