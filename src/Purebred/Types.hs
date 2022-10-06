-- This file is part of purebred
-- Copyright (C) 2017-2021 Róman Joost and Fraser Tweedale
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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

{- |

Core types and optics for Purebred.

-}
module Purebred.Types
  ( -- * Application state
    AppState(..)
  , asConfig
  , bChan
  , storageServer
  , logSink
  , asThreadsView
  , asMailView
  , asCompose
  , asUserMessage
  , asViews
  , asFileBrowser
  , asLocalTime
  , Async(..)
  , asAsync

    -- ** Threads and Mails Lists
  , ThreadsView(..)
  , miListOfMails
  , miListOfThreads
  , miListOfThreadsGeneration
  , miSearchThreadsEditor
  , miMailTagsEditor
  , miThreadTagsEditor
  , miThreads
  , miNewMail
  , miMails
  , NotmuchMail(..)
  , mailSubject
  , mailFrom
  , mailDate
  , mailTags
  , mailId
  , NotmuchThread(..)
  , thSubject
  , thAuthors
  , thDate
  , thTags
  , thReplies
  , thId

    -- ** Mail Viewer
  , MailView(..)
  , mvMail
  , mvBody
  , mvAttachments
  , mvSaveToDiskPath
  , mvOpenCommand
  , mvPipeCommand
  , mvFindWordEditor
  , mvSearchIndex
  , MailBody(..)
  , mbLines
  , mbMatches
  , mbSource
  , Source
  , Match(..)

    -- ** Mail Composer
  , Compose(..)
  , cFrom
  , cTo
  , cCc
  , cBcc
  , cSubject
  , cAttachments
  , cKeepDraft
  , ConfirmDraft(..)

    -- ** File Browser
  , FileBrowser(..)
  , fbEntries
  , fbSearchPath
  , FileSystemEntry(..)
  , fsEntryName

    -- ** Concurrent actions
  , aValidation

    -- ** Widgets
  , HeadersState(..)

    -- ** Keybindings
  , Keybinding(..)
  , kbEvent
  , kbAction
  , Action(..)
  , aDescription
  , aAction

    -- * Configuration
  , UserConfiguration
  , Configuration(..)
  , confTheme
  , confNotmuch
  , confEditor
  , confMailView
  , confIndexView
  , confComposeView
  , confHelpView
  , confDefaultView
  , confFileBrowserView
  , confCharsets
  , confPlugins

    -- ** Notmuch Configuration
  , NotmuchSettings(..)
  , nmSearch
  , nmDatabase
  , nmNewTag
  , nmDraftTag
  , nmSentTag
  , nmHasNewMailSearch
  , nmHasNewMailCheckDelay
  , Delay(..)
  , Tag

  -- ** Mail Viewer
  , MailViewSettings(..)
  , mvIndexRows
  , mvTextWidth
  , mvHeadersToShow
  , mvPreferredContentType
  , mvHeadersState
  , mvMailcap

  -- *** Mail Viewer Keybindings
  , mvKeybindings
  , mvManageMailTagsKeybindings
  , mvMailListOfAttachmentsKeybindings
  , mvOpenWithKeybindings
  , mvPipeToKeybindings
  , mvFindWordEditorKeybindings
  , mvSaveToDiskKeybindings
  , mvToKeybindings

  -- ** Threads Viewer
  , IndexViewSettings(..)
  -- *** Threads Viewer Keybindings
  , ivBrowseThreadsKeybindings
  , ivSearchThreadsKeybindings
  , ivManageThreadTagsKeybindings
  , ivFromKeybindings
  , ivToKeybindings
  , ivSubjectKeybindings

  -- ** Mail Composer
  , ComposeViewSettings(..)
  , cvSendMailCmd
  , cvIdentities
  -- *** Mail Composer Keybindings
  , cvFromKeybindings
  , cvToKeybindings
  , cvCcKeybindings
  , cvBccKeybindings
  , cvSubjectKeybindings
  , cvListOfAttachmentsKeybindings
  , cvConfirmKeybindings
  
  -- ** Help Viewer
  , HelpViewSettings(..)
  , hvKeybindings

  -- ** File Browser
  , FileBrowserSettings(..)
  , fbHomePath
  -- *** Keybindings
  , fbKeybindings
  , fbSearchPathKeybindings

  -- * Internals
  , ListWithLength(..)
  , listList
  , listLength

  , module Purebred.Types.Event
  , module Purebred.Types.UI
  , module Purebred.Types.String
  ) where

import Prelude hiding (Word)

import GHC.Generics (Generic)

import Brick.AttrMap (AttrMap)
import Brick.BChan (BChan)
import Brick.Types (EventM)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.FileBrowser as FB
import Brick.Widgets.Dialog (Dialog)
import Control.Lens ( Getter, Lens', Traversal', lens, to )
import Control.DeepSeq (NFData(rnf), force)
import Control.Concurrent (ThreadId)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Graphics.Vty.Input.Events as Vty
import Data.Time (UTCTime)
import qualified Data.CaseInsensitive as CI
import qualified Data.Vector as V

import Notmuch (Tag)
import Data.MIME

import Purebred.UI.Widgets (StatefulEditor)
import {-# SOURCE #-} Purebred.Plugin.Internal
import Purebred.Storage.Server
import Purebred.Types.Error
import Purebred.Types.Event
import Purebred.Types.Items
import Purebred.Types.Mailcap
import Purebred.Types.UI
import Purebred.Types.String

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}


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
{-# ANN listList ("HLint: ignore Avoid lambda using `infix`" :: String) #-}

listLength :: Lens' (ListWithLength t a) (Maybe Int)
listLength f (ListWithLength a b) = (\b' -> ListWithLength a b') <$> f b


-- | A view showing a list of threads.
-- This is the default view of the Purebred on startup.
--
data ThreadsView = ThreadsView
    { _miListOfMails  :: ListWithLength V.Vector (Toggleable NotmuchMail)
    , _miListOfThreads :: ListWithLength Items (Toggleable NotmuchThread)
    , _miListOfThreadsGeneration :: Generation
    , _miSearchThreadsEditor :: StatefulEditor T.Text Name
    , _miMailTagsEditor :: E.Editor T.Text Name
    , _miThreadTagsEditor :: E.Editor T.Text Name
    , _miNewMail :: Int
    }

miMails :: Lens' ThreadsView (ListWithLength V.Vector (Toggleable NotmuchMail))
miMails = lens _miListOfMails (\m v -> m { _miListOfMails = v })

miThreads :: Lens' ThreadsView (ListWithLength Items (Toggleable NotmuchThread))
miThreads = lens _miListOfThreads (\m v -> m { _miListOfThreads = v})

miListOfMails :: Lens' ThreadsView (L.GenericList Name V.Vector (Toggleable NotmuchMail))
miListOfMails = miMails . listList

miListOfThreads :: Lens' ThreadsView (L.GenericList Name Items (Toggleable NotmuchThread))
miListOfThreads = miThreads . listList

miListOfThreadsGeneration :: Lens' ThreadsView Generation
miListOfThreadsGeneration =
  lens _miListOfThreadsGeneration (\s b -> s { _miListOfThreadsGeneration = b })

miSearchThreadsEditor :: Lens' ThreadsView (StatefulEditor T.Text Name)
miSearchThreadsEditor = lens _miSearchThreadsEditor (\m v -> m { _miSearchThreadsEditor = v})

miMailTagsEditor :: Lens' ThreadsView (E.Editor T.Text Name)
miMailTagsEditor = lens _miMailTagsEditor (\m v -> m { _miMailTagsEditor = v})

miThreadTagsEditor :: Lens' ThreadsView (E.Editor T.Text Name)
miThreadTagsEditor = lens _miThreadTagsEditor (\m v -> m { _miThreadTagsEditor = v})

miNewMail :: Lens' ThreadsView Int
miNewMail = lens _miNewMail (\m v -> m { _miNewMail = v})

-- | A loose annotation what produced the rendered output of the
-- entity
--
type Source = T.Text

-- | Type representing a specific entity from an e-mail for display,
-- optionally with search matches to be highlighted.
--
data MailBody =
  MailBody Source [Match] [T.Text]
  deriving (Show, Eq)

mbLines :: Traversal' MailBody T.Text
mbLines f (MailBody s ms xs) = fmap (\xs' -> MailBody s ms xs') (traverse f xs)

mbMatches :: Lens' MailBody [Match]
mbMatches f (MailBody s ms xs) = fmap (\ms' -> MailBody s ms' xs) (f ms)

mbSource :: Lens' MailBody Source
mbSource f (MailBody s ms xs) = fmap (\s' -> MailBody s' ms xs) (f s)

-- | A match of a substring in the current line of text
--
data Match =
  Match Int -- ^ offset
        Int -- ^ length
        Int -- ^ line number
  deriving (Show, Eq)

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
    , _mvSearchIndex :: Int
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

mvSearchIndex :: Lens' MailView Int
mvSearchIndex = lens _mvSearchIndex (\mv i -> mv { _mvSearchIndex = i })

mvBody :: Lens' MailView MailBody
mvBody = lens _mvBody (\mv hs -> mv { _mvBody = hs })

data ConfirmDraft
  = Keep
  | Discard
  deriving (Show)

data Compose = Compose
    { _cFrom :: StatefulEditor T.Text Name
    , _cTo :: StatefulEditor T.Text Name
    , _cCc :: StatefulEditor T.Text Name
    , _cBcc :: StatefulEditor T.Text Name
    , _cSubject :: StatefulEditor T.Text Name
    , _cAttachments :: L.List Name MIMEMessage
    , _cKeepDraft :: Dialog ConfirmDraft
    }

cFrom :: Lens' Compose (StatefulEditor T.Text Name)
cFrom = lens _cFrom (\c x -> c { _cFrom = x })

cTo :: Lens' Compose (StatefulEditor T.Text Name)
cTo = lens _cTo (\c x -> c { _cTo = x })

cCc :: Lens' Compose (StatefulEditor T.Text Name)
cCc = lens _cCc (\c x -> c { _cCc = x })

cBcc :: Lens' Compose (StatefulEditor T.Text Name)
cBcc = lens _cBcc (\c x -> c { _cBcc = x })

cSubject :: Lens' Compose (StatefulEditor T.Text Name)
cSubject = lens _cSubject (\c x -> c { _cSubject = x })

cAttachments :: Lens' Compose (L.List Name MIMEMessage)
cAttachments = lens _cAttachments (\c x -> c { _cAttachments = x })

cKeepDraft :: Lens' Compose (Dialog ConfirmDraft)
cKeepDraft = lens _cKeepDraft (\c x -> c { _cKeepDraft = x })

data NotmuchSettings =
  NotmuchSettings
    { _nmSearch :: T.Text -- ^ The default query used on startup.
    , _nmDatabase :: FilePath -- ^ The 'FilePath' to the database.
    , _nmNewTag :: Tag -- ^ The 'Tag' indicating a new mail or thread.
    , _nmDraftTag :: Tag -- ^ The 'Tag' to attach mails during composition when saved as drafts.
    , _nmSentTag :: Tag -- ^ The 'Tag' to attach to mails once successfully sent.
    , _nmHasNewMailSearch :: T.Text -- ^ Search carried out by Purebred to determine the number of new mail.
    , _nmHasNewMailCheckDelay :: Maybe Delay
    -- ^ The interval in which Purebred queries for new mail. Set to 'Nothing' to disable
    -- the check.
    }
  deriving (Generic, NFData)

nmSearch :: Lens' NotmuchSettings T.Text
nmSearch = lens _nmSearch (\nm x -> nm { _nmSearch = x })

nmDatabase :: Lens' NotmuchSettings FilePath
nmDatabase = lens _nmDatabase (\nm x -> nm { _nmDatabase = x })

nmNewTag :: Lens' NotmuchSettings Tag
nmNewTag = lens _nmNewTag (\nm x -> nm { _nmNewTag = x })

nmDraftTag :: Lens' NotmuchSettings Tag
nmDraftTag = lens _nmDraftTag (\nm x -> nm { _nmDraftTag = x })

nmSentTag :: Lens' NotmuchSettings Tag
nmSentTag = lens _nmSentTag (\nm x -> nm { _nmSentTag = x })

nmHasNewMailSearch :: Lens' NotmuchSettings T.Text
nmHasNewMailSearch = lens _nmHasNewMailSearch (\nm x -> nm { _nmHasNewMailSearch = x })

nmHasNewMailCheckDelay :: Lens' NotmuchSettings (Maybe Delay)
nmHasNewMailCheckDelay = lens _nmHasNewMailCheckDelay (\nm x -> nm { _nmHasNewMailCheckDelay = x })

data FileBrowserSettings = FileBrowserSettings
  { _fbKeybindings :: [Keybinding 'FileBrowser 'ListOfFiles]
  , _fbSearchPathKeybindings :: [Keybinding 'FileBrowser 'ManageFileBrowserSearchPath]
  , _fbHomePath :: FilePath
  }
  deriving (Generic, NFData)

fbKeybindings :: Lens' FileBrowserSettings [Keybinding 'FileBrowser 'ListOfFiles]
fbKeybindings = lens _fbKeybindings (\cv x -> cv { _fbKeybindings = x })

fbSearchPathKeybindings :: Lens' FileBrowserSettings [Keybinding 'FileBrowser 'ManageFileBrowserSearchPath]
fbSearchPathKeybindings = lens _fbSearchPathKeybindings (\cv x -> cv { _fbSearchPathKeybindings = x})

fbHomePath :: Lens' FileBrowserSettings FilePath
fbHomePath = lens _fbHomePath (\s a -> s { _fbHomePath = a })

data Delay
  = Seconds Int
  | Minutes Int
  deriving (Generic, NFData)

type UserConfiguration = Configuration

data Configuration = Configuration
    { _confTheme :: AttrMap
    , _confNotmuch :: NotmuchSettings
    , _confEditor :: FilePath
    , _confMailView :: MailViewSettings
    , _confIndexView :: IndexViewSettings
    , _confComposeView :: ComposeViewSettings
    , _confHelpView :: HelpViewSettings
    , _confDefaultView :: ViewName
    , _confFileBrowserView :: FileBrowserSettings
    , _confCharsets :: CharsetLookup
    , _confPlugins :: [PluginDict]
    }
    deriving (Generic, NFData)

confTheme :: Lens' Configuration AttrMap
confTheme = lens _confTheme (\c x -> c { _confTheme = x })

confEditor :: Lens' Configuration FilePath
confEditor = lens _confEditor (\conf x -> conf { _confEditor = x })

confNotmuch :: Lens' Configuration NotmuchSettings
confNotmuch = lens _confNotmuch (\conf x -> conf { _confNotmuch = x })

confMailView :: Lens' Configuration MailViewSettings
confMailView = lens _confMailView (\conf x -> conf { _confMailView = x })

confIndexView :: Lens' Configuration IndexViewSettings
confIndexView = lens _confIndexView (\conf x -> conf { _confIndexView = x })

confComposeView :: Lens' Configuration ComposeViewSettings
confComposeView = lens _confComposeView (\conf x -> conf { _confComposeView = x})

confHelpView :: Lens' Configuration HelpViewSettings
confHelpView = lens _confHelpView (\conf x -> conf { _confHelpView = x })

confDefaultView :: Lens' Configuration ViewName
confDefaultView = lens _confDefaultView (\conf x -> conf { _confDefaultView = x })

confFileBrowserView :: Lens' Configuration FileBrowserSettings
confFileBrowserView = lens _confFileBrowserView (\conf x -> conf { _confFileBrowserView = x })

confCharsets :: Lens' Configuration CharsetLookup
confCharsets = lens _confCharsets (\conf x -> conf { _confCharsets = x })

confPlugins :: Lens' Configuration [PluginDict]
confPlugins = lens _confPlugins (\conf x -> conf { _confPlugins = x })


data ComposeViewSettings = ComposeViewSettings
    { _cvFromKeybindings :: [Keybinding 'ComposeView 'ComposeFrom]
    , _cvToKeybindings :: [Keybinding 'ComposeView 'ComposeTo]
    , _cvCcKeybindings :: [Keybinding 'ComposeView 'ComposeCc]
    , _cvBccKeybindings :: [Keybinding 'ComposeView 'ComposeBcc]
    , _cvSubjectKeybindings :: [Keybinding 'ComposeView 'ComposeSubject]
    , _cvSendMailCmd :: B.Builder -> IO (Either Error ())
    , _cvListOfAttachmentsKeybindings :: [Keybinding 'ComposeView 'ComposeListOfAttachments]
    , _cvIdentities :: [Mailbox]
    , _cvConfirmKeybindings :: [Keybinding 'ComposeView 'ConfirmDialog]
    }
    deriving (Generic, NFData)

cvFromKeybindings :: Lens' ComposeViewSettings [Keybinding 'ComposeView 'ComposeFrom]
cvFromKeybindings = lens _cvFromKeybindings (\cv x -> cv { _cvFromKeybindings = x })

cvToKeybindings :: Lens' ComposeViewSettings [Keybinding 'ComposeView 'ComposeTo]
cvToKeybindings = lens _cvToKeybindings (\cv x -> cv { _cvToKeybindings = x })

cvCcKeybindings :: Lens' ComposeViewSettings [Keybinding 'ComposeView 'ComposeCc]
cvCcKeybindings = lens _cvCcKeybindings (\cv x -> cv { _cvCcKeybindings = x })

cvBccKeybindings :: Lens' ComposeViewSettings [Keybinding 'ComposeView 'ComposeBcc]
cvBccKeybindings = lens _cvBccKeybindings (\cv x -> cv { _cvBccKeybindings = x })

cvSubjectKeybindings :: Lens' ComposeViewSettings [Keybinding 'ComposeView 'ComposeSubject]
cvSubjectKeybindings = lens _cvSubjectKeybindings (\cv x -> cv { _cvSubjectKeybindings = x })

cvSendMailCmd :: Lens' ComposeViewSettings (B.Builder -> IO (Either Error ()))
cvSendMailCmd = lens _cvSendMailCmd (\cv x -> cv { _cvSendMailCmd = x })

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
    -- used for forwarding mails
    , _mvToKeybindings :: [Keybinding 'ViewMail 'ComposeTo]
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

mvToKeybindings :: Lens' MailViewSettings [Keybinding 'ViewMail 'ComposeTo]
mvToKeybindings = lens _mvToKeybindings (\s x -> s { _mvToKeybindings = x })

data FileSystemEntry
    = Directory String
    | File String
    deriving (Show,Ord,Eq)

fsEntryName :: Getter FileSystemEntry String
fsEntryName = let toName (Directory n) = n
                  toName (File n) = n
              in to toName

data FileBrowser = CreateFileBrowser
  { _fbEntries :: FB.FileBrowser Name
  , _fbSearchPath :: StatefulEditor FilePath Name
  }

fbEntries :: Lens' FileBrowser (FB.FileBrowser Name)
fbEntries = lens _fbEntries (\cv x -> cv { _fbEntries = x })

fbSearchPath :: Lens' FileBrowser (StatefulEditor FilePath Name)
fbSearchPath = lens _fbSearchPath (\c x -> c { _fbSearchPath = x})

-- | State needed to be kept for keeping track of
-- concurrent/asynchronous actions
newtype Async = Async
  { _aValidation :: Maybe ThreadId
  }

aValidation :: Lens' Async (Maybe ThreadId)
aValidation = lens _aValidation (\as x -> as { _aValidation = x })


-- | The application state holding state to render widgets, error
-- management, as well as views and more.
--
data AppState = AppState
    { _asConfig :: Configuration
    , _bChan :: BChan PurebredEvent
    , _storageServer :: Purebred.Storage.Server.Server
    , _logSink :: LT.Text -> IO ()
    , _asThreadsView :: ThreadsView
    , _asMailView  :: MailView
    , _asCompose   :: Compose  -- ^ state to keep when user creates a new mail
    , _asUserMessage :: Maybe UserMessage
    , _asViews     :: ViewSettings -- ^ stores widget and focus information
    , _asFileBrowser :: FileBrowser
    , _asLocalTime :: UTCTime
    , _asAsync :: Async
    }

asConfig :: Lens' AppState Configuration
asConfig = lens _asConfig (\appstate x -> appstate { _asConfig = x })

bChan :: Lens' AppState (BChan PurebredEvent)
bChan = lens _bChan (\s a -> s { _bChan = a })

storageServer :: Lens' AppState Purebred.Storage.Server.Server
storageServer = lens _storageServer (\s a -> s { _storageServer = a })

logSink :: Lens' AppState (LT.Text -> IO ())
logSink = lens _logSink (\s a -> s { _logSink = a })

asThreadsView :: Lens' AppState ThreadsView
asThreadsView = lens _asThreadsView (\appstate x -> appstate { _asThreadsView = x })

asMailView :: Lens' AppState MailView
asMailView = lens _asMailView (\appstate x -> appstate { _asMailView = x })

asCompose :: Lens' AppState Compose
asCompose = lens _asCompose (\appstate x -> appstate { _asCompose = x })

asUserMessage :: Lens' AppState (Maybe UserMessage)
asUserMessage = lens _asUserMessage (\appstate x -> appstate { _asUserMessage = x })

asViews :: Lens' AppState ViewSettings
asViews = lens _asViews (\appstate x -> appstate { _asViews = x })

asFileBrowser :: Lens' AppState FileBrowser
asFileBrowser = lens _asFileBrowser (\as x -> as { _asFileBrowser = x })

asLocalTime :: Lens' AppState UTCTime
asLocalTime = lens _asLocalTime (\as x -> as { _asLocalTime = x })

asAsync :: Lens' AppState Async
asAsync = lens _asAsync (\as x -> as { _asAsync = x })

data Action (v :: ViewName) (ctx :: Name) a = Action
    { _aDescription :: [T.Text]
    -- ^ sequential list of things that the action does
    , _aAction :: EventM Name AppState a
    }

instance NFData (Action v ctx a) where
  rnf (Action desc m) = Action (force desc) m `seq` ()

instance Functor (Action v ctx) where
  fmap f (Action desc go) = Action desc (fmap f go)

instance Applicative (Action v ctx) where
  pure a = Action [] (pure a)
  Action desc1 f <*> Action desc2 a = Action (desc1 <> desc2) (f <*> a)

aAction :: Getter (Action v ctx a) (EventM Name AppState a)
aAction = to (\(Action _ b) -> b)

aDescription :: Getter (Action v ctx a) [T.Text]
aDescription = to (\(Action a _ ) -> a)


data Keybinding (v :: ViewName) (ctx :: Name) = Keybinding
    { _kbEvent :: Vty.Event
    , _kbAction :: Action v ctx ()
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

kbAction :: Getter (Keybinding v ctx) (Action v ctx ())
kbAction = to (\(Keybinding _ c) -> c)

-- | An email from the notmuch database represented in Purebred.
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

-- | A thread of mails from the notmuch database represented in Purebred.
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
