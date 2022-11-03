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

{-# LANGUAGE OverloadedStrings #-}

module Purebred.Config
  ( defaultConfig
  , sendmail
  , solarizedDark
  , getDatabasePath
  , tagReplacementMapAscii
  , tagReplacementMapEmoji
  ) where

import Control.Applicative ((<|>), liftA2)
import Data.List.NonEmpty (fromList)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))

import Control.Lens (set)
import Control.Monad.Except (runExceptT)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import qualified Data.Text as T
import System.Directory (getHomeDirectory)

import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Dialog as D
import Brick.Util (fg, on, bg)
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V

import Data.MIME (contentTypeTextPlain, defaultCharsets, matchContentType)

import Purebred.UI.FileBrowser.Keybindings
       (fileBrowserKeybindings, manageSearchPathKeybindings)
import Purebred.UI.GatherHeaders.Keybindings
       (gatherFromKeybindings,
        gatherToKeybindings,
        gatherSubjectKeybindings)
import Purebred.UI.Index.Keybindings
       (browseThreadsKeybindings, searchThreadsKeybindings, manageThreadTagsKeybindings)
import Purebred.UI.Mail.Keybindings
       (displayMailKeybindings, mailViewManageMailTagsKeybindings,
        mailAttachmentsKeybindings, openWithKeybindings,
        pipeToKeybindings, findWordEditorKeybindings,
        saveToDiskKeybindings, mailviewComposeToKeybindings)
import Purebred.UI.Help.Keybindings (helpKeybindings)
import Purebred.UI.ComposeEditor.Keybindings
       (listOfAttachmentsKeybindings, composeFromKeybindings,
        composeToKeybindings, composeSubjectKeybindings, confirmKeybindings,
        composeCcKeybindings, composeBccKeybindings)

import Purebred.Types
import Purebred.Types.Mailcap
import Purebred.Plugin.Internal
import qualified Purebred.Plugin.UserAgent
import Purebred.System.Process
import Purebred.Types.IFC (sanitiseText, untaint)
import Purebred.Types.Error
import Purebred.UI.Attr

-- | Invoke a @sendmail(1)@-compatible program to send the email
--
sendmail ::
     FilePath
  -> B.Builder -- ^ the rendered mail
  -> IO (Either Error ())
sendmail bin m = do
  -- -t which extracts recipients from the mail
  result <- runExceptT $ tryReadProcessStderr config
  pure $ case result of
    Left e -> Left $ SendMailError (show e)
    Right (ExitFailure _, stderr) -> Left $ SendMailError (untaint decode stderr)
    Right (ExitSuccess, _) -> Right ()
  where
    config = setStdin (byteStringInput (B.toLazyByteString m)) $ proc bin ["-t", "-v"]
    decode = T.unpack . sanitiseText . decodeLenient . L.toStrict

toggledColour :: V.Attr
toggledColour = fg V.cyan

-- | Default theme
solarizedDark :: A.AttrMap
solarizedDark =
    A.attrMap
        V.defAttr
        [ (listAttr, fg V.brightBlue)
        , (listSelectedAttr, V.black `on` V.yellow)
        , (listNewMailAttr, fg V.white)
        , (listSelectedNewmailAttr, fg V.white)
        , (listToggledAttr, toggledColour)
        , (listSelectedToggledAttr, bg V.red `V.withStyle` V.reverseVideo)
        , (mailTagAttr, fg V.cyan)
        , (mailTagToggledAttr, toggledColour)
        , (mailAuthorsAttr, fg V.brightBlue)
        , (mailNewmailAuthorsAttr, fg V.white)
        , (mailSelectedAuthorsAttr, fg V.black)
        , (mailSelectedNewmailAuthorsAttr, fg V.white)
        , (mailToggledAuthorsAttr, toggledColour)
        , (E.editFocusedAttr, fg V.white)
        , (editorAttr, fg V.brightBlue)
        , (editorLabelAttr, fg V.brightYellow)
        , (editorErrorAttr, fg V.red)
        , (statusbarErrorAttr, bg V.red)
        , (statusbarInfoAttr, bg V.green)
        , (statusbarWarningAttr, bg V.yellow)
        , (statusbarAttr, V.black `on` V.brightYellow)
        , (headerKeyAttr, fg V.cyan)
        , (headerValueAttr, fg V.brightCyan)
        , (helpTitleAttr, fg V.cyan `V.withStyle` V.bold)
        , (D.dialogAttr, V.yellow `on` V.white)
        , (D.buttonAttr, V.black `on` V.white)
        , (D.buttonSelectedAttr, bg V.green)
        , (textMatchHighlightAttr, V.white `on` V.green)
        , (currentTextMatchHighlightAttr, V.green `on` V.white)
        , (defaultAttr, V.defAttr)
        , (mailbodySourceAttr, fg V.blue)
        ]

-- | Returns the notmuch database path by executing 'notmuch config
-- get database.path' in a separate process.  If the process terminates
-- abnormally, returns an empty string.
--
getDatabasePath :: IO FilePath
getDatabasePath = do
  let cmd = "notmuch"
  let args = ["config", "get", "database.path"]
  (exitc, stdout, _err) <- readProcess $ proc cmd args
  pure $ case exitc of
    ExitFailure _ -> ""
    ExitSuccess -> filter (/= '\n') (untaint decode stdout)
  where
      decode = T.unpack . sanitiseText . decodeLenient . L.toStrict

-- | The default configuration used in Purebred.
--
-- Returns a default configuration, with some properties dependent on the
-- execution environment:
--
-- * Editor taken from @VISUAL@ or @EDITOR@ environment variable, falling
--   back to @"vi"@ if neither is defined.
--
-- * Notmuch database path determined by executing
--   @notmuch config get database.path@.
--
-- * Home directory for file browser taken from
--   'System.Directory.getHomeDirectory'.
--
-- Uses 'tagReplacementMapEmoji'.  You can override it with
-- 'tagReplacementMapAscii' or a replacement map of your own
-- choosing (including an empty map).
--
defaultConfig :: IO UserConfiguration
defaultConfig = do
  dbPath <- getDatabasePath
  editor <- fromMaybe "vi" <$> liftA2 (<|>) (lookupEnv "VISUAL") (lookupEnv "EDITOR")
  homeDir <- getHomeDirectory
  pure $
    Configuration
    { _confTheme = solarizedDark
    , _confNotmuch = NotmuchSettings
      { _nmSearch = "tag:inbox"
      , _nmDatabase = dbPath
      , _nmNewTag = "unread"
      , _nmDraftTag = "draft"
      , _nmSentTag = "sent"
      , _nmHasNewMailSearch = "tag:inbox and tag:unread"
      , _nmHasNewMailCheckDelay = Just (Seconds 3)
      }
    , _confEditor = editor
    , _confMailView = MailViewSettings
      { _mvIndexRows = 10
      , _mvTextWidth = 82
      , _mvPreferredContentType = contentTypeTextPlain
      , _mvHeadersToShow = (`elem` ["subject", "to", "from", "cc", "date"])
      , _mvKeybindings = displayMailKeybindings
      , _mvManageMailTagsKeybindings = mailViewManageMailTagsKeybindings
      , _mvMailListOfAttachmentsKeybindings = mailAttachmentsKeybindings
      , _mvOpenWithKeybindings = openWithKeybindings
      , _mvPipeToKeybindings = pipeToKeybindings
      , _mvFindWordEditorKeybindings = findWordEditorKeybindings
      , _mvSaveToDiskKeybindings = saveToDiskKeybindings
      , _mvToKeybindings = mailviewComposeToKeybindings
      , _mvMailcap =
          [ ( matchContentType "text" (Just "html")
            , MailcapHandler (Shell (fromList "elinks -force-html")) CopiousOutput DiscardTempfile)
          , ( const True
            , MailcapHandler (Process (fromList "xdg-open") []) IgnoreOutput KeepTempfile)
          ]
      }
    , _confIndexView = IndexViewSettings
      { _ivBrowseThreadsKeybindings = browseThreadsKeybindings
      , _ivSearchThreadsKeybindings = searchThreadsKeybindings
      , _ivManageThreadTagsKeybindings = manageThreadTagsKeybindings
      , _ivFromKeybindings = gatherFromKeybindings
      , _ivToKeybindings = gatherToKeybindings
      , _ivSubjectKeybindings = gatherSubjectKeybindings
      , _ivTagReplacementMap = tagReplacementMapEmoji
      }
    , _confComposeView = ComposeViewSettings
      { _cvFromKeybindings = composeFromKeybindings
      , _cvToKeybindings = composeToKeybindings
      , _cvCcKeybindings = composeCcKeybindings
      , _cvBccKeybindings = composeBccKeybindings
      , _cvSubjectKeybindings = composeSubjectKeybindings
      , _cvSendMailCmd = sendmail "/usr/sbin/sendmail"
      , _cvListOfAttachmentsKeybindings = listOfAttachmentsKeybindings
      , _cvIdentities = []
      , _cvConfirmKeybindings = confirmKeybindings
      }
    , _confHelpView = HelpViewSettings
      { _hvKeybindings = helpKeybindings
      }
    , _confDefaultView = Threads
    , _confFileBrowserView = FileBrowserSettings
      { _fbKeybindings = fileBrowserKeybindings
      , _fbSearchPathKeybindings = manageSearchPathKeybindings
      , _fbHomePath = homeDir
      }
    , _confCharsets = defaultCharsets
    , _confPlugins = set pluginBuiltIn True <$>
        [ usePlugin Purebred.Plugin.UserAgent.plugin
        ]
    }

-- | Replace some special tags with ASCII chars.
--
-- * @flagged@ → @!@
-- * @attachment@ → @A@
-- * @inbox@ → @I@
-- * @replied@ → @r@
--
tagReplacementMapAscii :: M.Map T.Text T.Text
tagReplacementMapAscii = M.fromList
  [ ("flagged", "!")
  , ("attachment", "A")
  , ("inbox", "I")
  , ("replied", "r")
  ]

-- | Replace some special tags with emoji.
--
-- * @flagged@ → 📌
-- * @attachment@ → 📎
-- * @inbox@ → 📥
-- * @replied@ → ↩️
--
tagReplacementMapEmoji :: M.Map T.Text T.Text
tagReplacementMapEmoji = M.fromList
  [ ("flagged", "📌")
  , ("attachment", "📎")
  , ("inbox", "📥")
  -- Note: there's a trailing space here because most terminals render
  -- it as a double-width char but only advance the cursor one position.
  --
  -- That is because this emoji is U+21A9 (↩) + U+FE0F (Variation selector
  -- 16) which in this context is "emoji presentation selector".  So
  -- terminals see it as a single-width character.
  --
  , ("replied", "↩️ ")
  ]
