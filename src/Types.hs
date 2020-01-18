-- This file is part of purebred
-- Copyright (C) 2017-2019 Róman Joost and Fraser Tweedale
--
-- purebred is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Basic types for the UI used by this library
module Types
  ( module Types
  , Tag
  ) where

import Prelude hiding (Word)

import GHC.Generics (Generic)

import Brick.AttrMap (AttrMap)
import Brick.BChan (BChan)
import qualified Brick.Focus as Brick
import Brick.Types (EventM, Next)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Brick.Widgets.Dialog (Dialog)
import Control.DeepSeq (NFData(rnf), force)
import Control.Lens
import qualified Data.Map as Map
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadIO)
import Control.Concurrent (ThreadId)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Vector as V
import qualified Graphics.Vty.Input.Events as Vty
import Data.Time (UTCTime)
import qualified Data.CaseInsensitive as CI
import Data.List.NonEmpty (NonEmpty)
import System.Exit (ExitCode(..))
import System.Process.Typed (ProcessConfig)

import Notmuch (Tag)
import Data.MIME

import Error
import Purebred.LazyVector (V)
import Purebred.Types.IFC (Tainted)

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}

-- | Used to identify widgets in brick
data Name =
    SearchThreadsEditor
    | ListOfMails
    | ListOfThreads
    | ScrollingMailView
    | ScrollingMailViewFindWordEditor
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
    | ConfirmDialog
    | SaveToDiskPathEditor
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
    , _miNewMail :: Int
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

miNewMail :: Lens' MailIndex Int
miNewMail = lens _miNewMail (\m v -> m { _miNewMail = v})

-- | A loose annotation what produced the rendered output of the
-- entity
--
type Source = T.Text

-- | Type representing a specific entity from an e-mail for display.
--
data MailBody =
  MailBody Source [Paragraph]
  deriving (Show, Eq)

mbParagraph :: Traversal' MailBody Paragraph
mbParagraph f (MailBody s xs) = fmap (\xs' -> MailBody s xs') (traverse f xs)

mbSource :: Lens' MailBody Source
mbSource f (MailBody d xs) = fmap (\d' -> MailBody d' xs) (f d)

matchCount :: MailBody -> Int
matchCount =
  foldrOf
    (mbParagraph . pLine)
    (\l amount -> view (lMatches . to length) l + amount)
    0

-- | A paragraph in the mail body
--
newtype Paragraph =
  Paragraph [Line]
  deriving (Show, Eq)

pLine :: Traversal' Paragraph Line
pLine f (Paragraph xs) = fmap (\xs' -> Paragraph xs') (traverse f xs)

-- | A match of a substring in the current line of text
--
data Match =
  Match Int -- ^ offset
        Int -- ^ length
        Int -- ^ line number
  deriving (Show, Eq)

mLinenumber :: Lens' Match Int
mLinenumber f (Match a b c) = fmap (\c' -> Match a b c') (f c)

-- | A scroll step indicated by the sequential number, line number and
-- the match.
-- The sequential number is used for visual purposes to render a
-- status like: 2 of 30 matches
--
type ScrollStep = (Int, Int, Match)

stNumber :: Lens' ScrollStep Int
stNumber = _1

stMatch :: Lens' ScrollStep Match
stMatch = _3

-- | A line of text with arbitrary length and possible matching sub
-- strings
--
data Line =
  Line [Match]
       Int -- ^ line number
       T.Text
  deriving (Show, Eq)

hasMatches :: Line -> Bool
hasMatches = notNullOf (lMatches . traverse)

lMatches :: Lens' Line [Match]
lMatches f (Line xs n t) = fmap (\xs' -> Line xs' n t) (f xs)

lText :: Lens' Line T.Text
lText f (Line xs n t) = fmap (\t' -> Line xs n t') (f t)

lNumber :: Lens' Line Int
lNumber f (Line xs n t) = fmap (\n' -> Line xs n' t) (f n)

data HeadersState = ShowAll | Filtered

data MailView = MailView
    { _mvMail :: Maybe MIMEMessage
    , _mvBody:: MailBody
    , _mvHeadersState :: HeadersState
    , _mvAttachments :: L.List Name WireEntity
    , _mvSaveToDiskPath :: E.Editor T.Text Name
    , _mvOpenCommand:: E.Editor T.Text Name
    , _mvPipeCommand :: E.Editor T.Text Name
    , _mvFindWordEditor :: E.Editor T.Text Name
    , _mvScrollSteps :: Brick.FocusRing ScrollStep
    }

mvMail :: Lens' MailView (Maybe MIMEMessage)
mvMail = lens _mvMail (\mv pm -> mv { _mvMail = pm })

mvHeadersState :: Lens' MailView HeadersState
mvHeadersState = lens _mvHeadersState (\mv hs -> mv { _mvHeadersState = hs })

mvAttachments :: Lens' MailView (L.List Name WireEntity)
mvAttachments = lens _mvAttachments (\mv hs -> mv { _mvAttachments = hs })

mvSaveToDiskPath :: Lens' MailView (E.Editor T.Text Name)
mvSaveToDiskPath = lens _mvSaveToDiskPath (\mv hs -> mv { _mvSaveToDiskPath = hs })

mvOpenCommand :: Lens' MailView (E.Editor T.Text Name)
mvOpenCommand = lens _mvOpenCommand (\mv hs -> mv { _mvOpenCommand = hs })

mvPipeCommand :: Lens' MailView (E.Editor T.Text Name)
mvPipeCommand = lens _mvPipeCommand (\mv hs -> mv { _mvPipeCommand = hs })

mvFindWordEditor :: Lens' MailView (E.Editor T.Text Name)
mvFindWordEditor = lens _mvFindWordEditor (\mv hs -> mv { _mvFindWordEditor = hs })

mvScrollSteps :: Lens' MailView (Brick.FocusRing ScrollStep)
mvScrollSteps = lens _mvScrollSteps (\mv hs -> mv { _mvScrollSteps = hs })

mvBody :: Lens' MailView MailBody
mvBody = lens _mvBody (\mv hs -> mv { _mvBody = hs })

data ConfirmDraft
  = Keep
  | Discard
  deriving (Show)

data Compose = Compose
    { _cFrom :: E.Editor T.Text Name
    , _cTo :: E.Editor T.Text Name
    , _cSubject :: E.Editor T.Text Name
    , _cTemp :: T.Text
    , _cAttachments :: L.List Name MIMEMessage
    , _cKeepDraft :: Dialog ConfirmDraft
    }

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

cKeepDraft :: Lens' Compose (Dialog ConfirmDraft)
cKeepDraft = lens _cKeepDraft (\c x -> c { _cKeepDraft = x })

data NotmuchSettings a = NotmuchSettings
    { _nmSearch :: T.Text
    , _nmDatabase :: a
    , _nmNewTag :: Tag
    , _nmDraftTag :: Tag
    , _nmSentTag :: Tag
    , _nmHasNewMailSearch :: T.Text
    , _nmHasNewMailCheckDelay :: Maybe Int
    }
    deriving (Generic, NFData)

nmSearch :: Lens' (NotmuchSettings a) T.Text
nmSearch = lens _nmSearch (\nm x -> nm { _nmSearch = x })

nmDatabase :: Lens (NotmuchSettings a) (NotmuchSettings b) a b
nmDatabase = lens _nmDatabase (\nm x -> nm { _nmDatabase = x })

nmNewTag :: Lens' (NotmuchSettings a) Tag
nmNewTag = lens _nmNewTag (\nm x -> nm { _nmNewTag = x })

nmDraftTag :: Lens' (NotmuchSettings a) Tag
nmDraftTag = lens _nmDraftTag (\nm x -> nm { _nmDraftTag = x })

nmSentTag :: Lens' (NotmuchSettings a) Tag
nmSentTag = lens _nmSentTag (\nm x -> nm { _nmSentTag = x })

nmHasNewMailSearch :: Lens' (NotmuchSettings a) T.Text
nmHasNewMailSearch = lens _nmHasNewMailSearch (\nm x -> nm { _nmHasNewMailSearch = x })

nmHasNewMailCheckDelay :: Lens' (NotmuchSettings a) (Maybe Int)
nmHasNewMailCheckDelay = lens _nmHasNewMailCheckDelay (\nm x -> nm { _nmHasNewMailCheckDelay = x })

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
    { _confTheme :: AttrMap
    , _confNotmuch :: NotmuchSettings a
    , _confEditor :: b
    , _confMailView :: MailViewSettings
    , _confIndexView :: IndexViewSettings
    , _confComposeView :: ComposeViewSettings
    , _confHelpView :: HelpViewSettings
    , _confDefaultView :: ViewName
    , _confFileBrowserView :: FileBrowserSettings c
    , _confCharsets :: CharsetLookup
    , _confExtra :: extra  -- data specific to a particular "phase" of configuration
    }
    deriving (Generic, NFData)

type UserConfiguration = Configuration () (IO FilePath) (IO String) (IO FilePath)
type InternalConfiguration = Configuration (BChan PurebredEvent, String) FilePath String FilePath

type ConfigurationLens v = forall z a b c. Lens' (Configuration z a b c) v

confTheme :: ConfigurationLens AttrMap
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

confCharsets :: ConfigurationLens CharsetLookup
confCharsets = lens _confCharsets (\conf x -> conf { _confCharsets = x })

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
    , _cvSendMailCmd :: FilePath -> B.ByteString -> IO (Either Error ())
    , _cvSendMailPath :: FilePath
    , _cvListOfAttachmentsKeybindings :: [Keybinding 'ComposeView 'ComposeListOfAttachments]
    , _cvIdentities :: [Mailbox]
    , _cvConfirmKeybindings :: [Keybinding 'ComposeView 'ConfirmDialog]
    }
    deriving (Generic, NFData)

cvFromKeybindings :: Lens' ComposeViewSettings [Keybinding 'ComposeView 'ComposeFrom]
cvFromKeybindings = lens _cvFromKeybindings (\cv x -> cv { _cvFromKeybindings = x })

cvToKeybindings :: Lens' ComposeViewSettings [Keybinding 'ComposeView 'ComposeTo]
cvToKeybindings = lens _cvToKeybindings (\cv x -> cv { _cvToKeybindings = x })

cvSubjectKeybindings :: Lens' ComposeViewSettings [Keybinding 'ComposeView 'ComposeSubject]
cvSubjectKeybindings = lens _cvSubjectKeybindings (\cv x -> cv { _cvSubjectKeybindings = x })

cvSendMailCmd :: Lens' ComposeViewSettings (FilePath -> B.ByteString -> IO (Either Error ()))
cvSendMailCmd = lens _cvSendMailCmd (\cv x -> cv { _cvSendMailCmd = x })

cvSendMailPath :: Lens' ComposeViewSettings FilePath
cvSendMailPath = lens _cvSendMailPath (\cv x -> cv { _cvSendMailPath = x })

cvListOfAttachmentsKeybindings :: Lens' ComposeViewSettings [Keybinding 'ComposeView 'ComposeListOfAttachments]
cvListOfAttachmentsKeybindings = lens _cvListOfAttachmentsKeybindings (\cv x -> cv { _cvListOfAttachmentsKeybindings = x })

cvIdentities :: Lens' ComposeViewSettings [Mailbox]
cvIdentities = lens _cvIdentities (\cv x -> cv { _cvIdentities = x })

cvConfirmKeybindings :: Lens' ComposeViewSettings [Keybinding 'ComposeView 'ConfirmDialog]
cvConfirmKeybindings = lens _cvConfirmKeybindings (\cv x -> cv { _cvConfirmKeybindings = x })

newtype HelpViewSettings = HelpViewSettings
  { _hvKeybindings :: [Keybinding 'Help 'ScrollingHelpView]
  }
  deriving (Generic, NFData)

hvKeybindings :: Lens' HelpViewSettings [Keybinding 'Help 'ScrollingHelpView]
hvKeybindings f (HelpViewSettings a) = fmap (\a' -> HelpViewSettings a') (f a)

data IndexViewSettings = IndexViewSettings
    { _ivBrowseThreadsKeybindings :: [Keybinding 'Threads 'ListOfThreads]
    , _ivSearchThreadsKeybindings :: [Keybinding 'Threads 'SearchThreadsEditor]
    , _ivManageThreadTagsKeybindings :: [Keybinding 'Threads 'ManageThreadTagsEditor]
    , _ivFromKeybindings :: [Keybinding 'Threads 'ComposeFrom]
    , _ivToKeybindings :: [Keybinding 'Threads 'ComposeTo]
    , _ivSubjectKeybindings :: [Keybinding 'Threads 'ComposeSubject]
    }
    deriving (Generic, NFData)

ivBrowseThreadsKeybindings :: Lens' IndexViewSettings [Keybinding 'Threads 'ListOfThreads]
ivBrowseThreadsKeybindings = lens _ivBrowseThreadsKeybindings (\s x -> s { _ivBrowseThreadsKeybindings = x })

ivSearchThreadsKeybindings :: Lens' IndexViewSettings [Keybinding 'Threads 'SearchThreadsEditor]
ivSearchThreadsKeybindings = lens _ivSearchThreadsKeybindings (\s x -> s { _ivSearchThreadsKeybindings = x })

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
    , _mvTextWidth :: Int
    , _mvPreferredContentType :: ContentType
    , _mvHeadersToShow       :: CI.CI B.ByteString -> Bool
    , _mvKeybindings         :: [Keybinding 'ViewMail 'ScrollingMailView]
    , _mvManageMailTagsKeybindings :: [Keybinding 'ViewMail 'ManageMailTagsEditor]
    , _mvMailListOfAttachmentsKeybindings :: [Keybinding 'ViewMail 'MailListOfAttachments]
    , _mvOpenWithKeybindings :: [Keybinding 'ViewMail 'MailAttachmentOpenWithEditor]
    , _mvPipeToKeybindings :: [Keybinding 'ViewMail 'MailAttachmentPipeToEditor]
    , _mvFindWordEditorKeybindings :: [Keybinding 'ViewMail 'ScrollingMailViewFindWordEditor]
    , _mvMailcap :: [(ContentType -> Bool, MailcapHandler)]
    , _mvSaveToDiskKeybindings :: [Keybinding 'ViewMail 'SaveToDiskPathEditor]
    }
    deriving (Generic, NFData)

mvIndexRows :: Lens' MailViewSettings Int
mvIndexRows = lens _mvIndexRows (\mv x -> mv { _mvIndexRows = x })

mvTextWidth :: Lens' MailViewSettings Int
mvTextWidth = lens _mvTextWidth (\mv x -> mv { _mvTextWidth = x })

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

mvFindWordEditorKeybindings :: Lens' MailViewSettings [Keybinding 'ViewMail 'ScrollingMailViewFindWordEditor]
mvFindWordEditorKeybindings = lens _mvFindWordEditorKeybindings (\s x -> s { _mvFindWordEditorKeybindings = x })

mvMailcap :: Lens' MailViewSettings [(ContentType -> Bool, MailcapHandler)]
mvMailcap = lens _mvMailcap (\s x -> s { _mvMailcap = x })

mvSaveToDiskKeybindings :: Lens' MailViewSettings [Keybinding 'ViewMail 'SaveToDiskPathEditor]
mvSaveToDiskKeybindings = lens _mvSaveToDiskKeybindings (\s x -> s { _mvSaveToDiskKeybindings = x })

hasCopiousoutput :: Traversal' [(ContentType -> Bool, MailcapHandler)] (ContentType -> Bool, MailcapHandler)
hasCopiousoutput = traversed . filtered (view (_2 . mhCopiousoutput . to isCopiousOutput))

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

type Layers = V.Vector Layer

data View = View
    { _vFocus :: Name
    , _vLayers :: Layers
    }

vLayers :: Lens' View Layers
vLayers = lens _vLayers (\settings x -> settings { _vLayers = x })

vFocus :: Lens' View Name
vFocus = lens _vFocus (\settings x -> settings { _vFocus = x})

-- A layer is a view element with a list of widgets and their view
-- state
newtype Layer =
  Layer (V.Vector Tile)
  deriving (Eq, Show)

type instance Index Layer = Name
type instance IxValue Layer = Tile

instance Ixed Layer where
  ix = tile

layeriso :: Iso' Layer (V.Vector Tile)
layeriso = iso (\(Layer xs) -> xs) Layer

tile :: Name -> Traversal' Layer Tile
tile k = layeriso . traversed . filtered (\x -> k == view veName x)

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

-- | State needed to be kept for keeping track of
-- concurrent/asynchronous actions
newtype Async = Async
  { _aValidation :: Maybe ThreadId
  }

aValidation :: Lens' Async (Maybe ThreadId)
aValidation = lens _aValidation (\as x -> as { _aValidation = x })


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
    , _asAsync :: Async
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
asViews f (AppState a b c d e g h i j) = fmap (\g' -> AppState a b c d e g' h i j) (f g)

asFileBrowser :: Lens' AppState FileBrowser
asFileBrowser = lens _asFileBrowser (\as x -> as { _asFileBrowser = x })

asLocalTime :: Lens' AppState UTCTime
asLocalTime = lens _asLocalTime (\as x -> as { _asLocalTime = x })

asAsync :: Lens' AppState Async
asAsync = lens _asAsync (\as x -> as { _asAsync = x })

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
data ResourceSpec m a = ResourceSpec
  { _rsAcquire :: (MonadIO m, MonadError Error m) =>
                    m a
 -- ^ acquire a resource (e.g. create temporary file)
  , _rsFree :: (MonadIO m, MonadError Error m) =>
                 a -> m ()
 -- ^ release a resource (e.g. remove temporary file)
  , _rsUpdate :: (MonadIO m, MonadError Error m) =>
                   a -> B.ByteString -> m ()
 -- ^ update the acquired resource with the ByteString obtained from serialising the WireEntity
  }

rsAcquire :: (MonadError Error m, MonadIO m) => Lens' (ResourceSpec m a) (m a)
rsAcquire = lens _rsAcquire (\rs x -> rs {_rsAcquire = x})

rsFree ::
     (MonadError Error m, MonadIO m) => Lens' (ResourceSpec m a) (a -> m ())
rsFree = lens _rsFree (\rs x -> rs {_rsFree = x})

rsUpdate ::
     (MonadError Error m, MonadIO m)
  => Lens' (ResourceSpec m a) (a -> B.ByteString -> m ())
rsUpdate = lens _rsUpdate (\rs x -> rs {_rsUpdate = x})

data MakeProcess
  = Shell (NonEmpty Char)
  | Process (NonEmpty Char)
            [String]
  deriving (Generic, NFData)

mpCommand :: Lens' MakeProcess (NonEmpty Char)
mpCommand f (Shell x) = fmap (\x' -> Shell x') (f x)
mpCommand f (Process x args) = fmap (\x' -> Process x' args) (f x)

data CopiousOutput
  = CopiousOutput
  | IgnoreOutput
  deriving (Generic, NFData)

isCopiousOutput :: CopiousOutput -> Bool
isCopiousOutput CopiousOutput = True
isCopiousOutput _ = False

data TempfileOnExit
  = KeepTempfile
  | DiscardTempfile
  deriving (Generic, NFData)

data MailcapHandler = MailcapHandler
  { _mhMakeProcess :: MakeProcess
  , _mhCopiousoutput :: CopiousOutput
  -- ^ output should be paged or made scrollable
  , _mhKeepTemp :: TempfileOnExit
  -- ^ Keep the temporary file if application spawns child and parent
  -- exits immediately (e.g. Firefox)
  } deriving (Generic, NFData)

mhMakeProcess :: Lens' MailcapHandler MakeProcess
mhMakeProcess = lens _mhMakeProcess (\h x -> h { _mhMakeProcess = x })

mhCopiousoutput :: Lens' MailcapHandler CopiousOutput
mhCopiousoutput = lens _mhCopiousoutput (\h x -> h { _mhCopiousoutput = x })

mhKeepTemp :: Lens' MailcapHandler TempfileOnExit
mhKeepTemp = lens _mhKeepTemp (\h x -> h { _mhKeepTemp = x })


-- | Command configuration which is bound to an acquired resource
-- (e.g. a tempfile) filtered through an external command. The
-- resource may or may not be cleaned up after the external command
-- exits.
data EntityCommand m a = EntityCommand
  { _ccAfterExit :: (MonadIO m, MonadError Error m) =>
                      (ExitCode, Tainted LB.ByteString) -> a -> m T.Text
  , _ccResource :: (MonadIO m, MonadError Error m) =>
                     ResourceSpec m a
  , _ccProcessConfig :: B.ByteString -> a -> ProcessConfig () () ()
  , _ccRunProcess :: (MonadError Error m, MonadIO m) =>
                       ProcessConfig () () () -> m ( ExitCode
                                                   , Tainted LB.ByteString)
  , _ccEntity :: B.ByteString
  -- ^ The decoded Entity
  }

ccAfterExit ::
     (MonadIO m, MonadError Error m)
  => Lens' (EntityCommand m a) ((ExitCode, Tainted LB.ByteString) -> a -> m T.Text)
ccAfterExit = lens _ccAfterExit (\cc x -> cc {_ccAfterExit = x})

ccEntity :: Lens' (EntityCommand m a) B.ByteString
ccEntity = lens _ccEntity (\cc x -> cc {_ccEntity = x})

ccProcessConfig ::
     Lens' (EntityCommand m a) (B.ByteString -> a -> ProcessConfig () () ())
ccProcessConfig = lens _ccProcessConfig (\cc x -> cc {_ccProcessConfig = x})

ccResource ::
     (MonadIO m, MonadError Error m)
  => Lens' (EntityCommand m a) (ResourceSpec m a)
ccResource = lens _ccResource (\cc x -> cc {_ccResource = x})

ccRunProcess ::
     (MonadError Error m, MonadIO m)
  => Lens' (EntityCommand m a) (ProcessConfig () () () -> m ( ExitCode
                                                            , Tainted LB.ByteString))
ccRunProcess = lens _ccRunProcess (\cc x -> cc {_ccRunProcess = x})

-- | A serial number that can be used to match (or ignore as
-- irrelevant) asynchronous events to current application state.
--
-- Use the 'Eq' and 'Ord' instances to compare generations.  The
-- constructor is hidden; use 'firstGeneration' as the first
-- generation, and use 'nextGeneration' to monotonically increment
-- it.
--
newtype Generation = Generation Integer
  deriving (Eq, Ord)

-- | Purebred event type.  In the future we can abstract this over
-- a custom event type to allow plugins to define their own events.
-- But I've YAGNI'd it for now because it will require an event
-- type parameter on 'AppState', which will be a noisy change.
--
data PurebredEvent
  = NotifyNumThreads Int
                     Generation
  | NotifyNewMailArrived Int
  | InputValidated (Lens' AppState (Maybe Error))
                   (Maybe Error)
