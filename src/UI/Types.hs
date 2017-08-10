-- | Basic types for the UI used by this library
module UI.Types where

import qualified Brick.AttrMap as Brick
import           Brick.Types               (EventM, Next)
import qualified Brick.Widgets.Edit        as E
import qualified Brick.Widgets.List        as L
import Control.Lens
import qualified Data.Text                 as T
import qualified Graphics.Vty.Input.Events as Vty
import           Storage.Mail              (Mail)
import           Storage.ParsedMail        (ParsedMail)


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

miListOfMails :: Lens' MailIndex (L.List Name Mail)
miListOfMails f (MailIndex a b c) = fmap (\a' -> MailIndex a' b c) (f a)

miSearchEditor :: Lens' MailIndex (E.Editor T.Text Name)
miSearchEditor f (MailIndex a b c) = fmap (\b' -> MailIndex a b' c) (f b)

miMode :: Lens' MailIndex MainMode
miMode f (MailIndex a b c) = fmap (\c' -> MailIndex a b c') (f c)


data MailView = MailView
    { _mvMail :: Maybe ParsedMail
    }

mvMail :: Iso' MailView (Maybe ParsedMail)
mvMail = iso (\(MailView a) -> a) MailView

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


data Configuration = Configuration
    { _confColorMap        :: Brick.AttrMap
    , _confNotmuchsearch   :: T.Text
    , _confNotmuchDatabase :: String
    , _confEditor          :: T.Text
    , _confMailView        :: MailViewSettings
    , _confIndexView       :: IndexView
    }

confColorMap :: Getter Configuration Brick.AttrMap
confColorMap = to (\(Configuration a _ _ _ _ _) -> a)

confNotmuchsearch :: Getter Configuration T.Text
confNotmuchsearch = to (\(Configuration _ b _ _ _ _) -> b)

confNotmuchDatabase :: Getter Configuration String
confNotmuchDatabase = to (\(Configuration _ _ c _ _ _) -> c)

confMailView :: Getter Configuration MailViewSettings
confMailView = to (\(Configuration _ _ _ _ e _) -> e)

confIndexView :: Getter Configuration IndexView
confIndexView = to (\(Configuration _ _ _ _ _ g) -> g)

newtype IndexView = IndexView
    { _ivKeybindings :: [Keybinding]
    }

ivKeybindings :: Lens' IndexView [Keybinding]
ivKeybindings f (IndexView a) = fmap (\a' -> IndexView a') (f a)

data MailViewSettings = MailViewSettings
    { _mvIndexRows           :: Int
    , _mvPreferedContentType :: T.Text
    , _mvHeadersToShow       :: [T.Text]
    }

mvIndexRows :: Lens' MailViewSettings Int
mvIndexRows f (MailViewSettings a b c) = fmap (\a' -> MailViewSettings a' b c) (f a)

mvPreferredContentType :: Lens' MailViewSettings T.Text
mvPreferredContentType f (MailViewSettings a b c) = fmap (\b' -> MailViewSettings a b' c) (f b)

mvHeadersToShow :: Lens' MailViewSettings [T.Text]
mvHeadersToShow f (MailViewSettings a b c) = fmap (\c' -> MailViewSettings a b c') (f c)

-- | Overall application state
data AppState = AppState
    { _asConfig    :: Configuration
    , _asMailIndex :: MailIndex
    , _asMailView  :: MailView
    , _asCompose   :: Compose  -- ^ state to keep when user creates a new mail
    , _asAppMode   :: Mode
    , _asError     :: Maybe String -- ^ in case of errors, show this error message
    }

asConfig :: Lens' AppState Configuration
asConfig f (AppState a b c d e g) = fmap (\a' -> AppState a' b c d e g) (f a)

asMailIndex :: Lens' AppState MailIndex
asMailIndex f (AppState a b c d e g) = fmap (\b' -> AppState a b' c d e g) (f b)

asMailView :: Lens' AppState MailView
asMailView f (AppState a b c d e g) = fmap (\c' -> AppState a b c' d e g) (f c)

asCompose :: Lens' AppState Compose
asCompose f (AppState a b c d e g) = fmap (\d' -> AppState a b c d' e g) (f d)

asAppMode :: Lens' AppState Mode
asAppMode f (AppState a b c d e g) = fmap (\e' -> AppState a b c d e' g) (f e)

asError :: Lens' AppState (Maybe String)
asError f (AppState a b c d e g) = fmap (\g' -> AppState a b c d e g') (f g)

type KBAction = AppState -> EventM Name (Next AppState)
data Keybinding = Keybinding
    { _kbDescription :: String
    , _kbEvent       :: Vty.Event
    , _kbAction      :: KBAction
    }

kbEvent :: Getter Keybinding Vty.Event
kbEvent = to (\(Keybinding _ b _) -> b)

kbAction :: Getter Keybinding KBAction
kbAction = to (\(Keybinding _ _ c) -> c)
