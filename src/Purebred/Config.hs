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

module Purebred.Config where

import qualified Brick.AttrMap as A
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Dialog as D
import Brick.Util (fg, on, bg)
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Control.Monad.Except (runExceptT)
import System.Environment (lookupEnv)
import System.Directory (getHomeDirectory)
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (fromList)
import System.Exit (ExitCode(..))

import Control.Lens (set)

import Data.MIME (contentTypeTextPlain, defaultCharsets, matchContentType)

import UI.FileBrowser.Keybindings
       (fileBrowserKeybindings, manageSearchPathKeybindings)
import UI.GatherHeaders.Keybindings
       (gatherFromKeybindings,
        gatherToKeybindings,
        gatherSubjectKeybindings)
import UI.Index.Keybindings
       (browseThreadsKeybindings, searchThreadsKeybindings, manageThreadTagsKeybindings)
import UI.Mail.Keybindings
       (displayMailKeybindings, mailViewManageMailTagsKeybindings,
        mailAttachmentsKeybindings, openWithKeybindings,
        pipeToKeybindings, findWordEditorKeybindings,
        saveToDiskKeybindings, mailviewComposeToKeybindings)
import UI.Help.Keybindings (helpKeybindings)
import UI.ComposeEditor.Keybindings
       (listOfAttachmentsKeybindings, composeFromKeybindings,
        composeToKeybindings, composeSubjectKeybindings, confirmKeybindings,
        composeCcKeybindings, composeBccKeybindings)

import Types
import Purebred.Plugin.Internal
import qualified Purebred.Plugin.UserAgent
import Purebred.Storage.Notmuch (getDatabasePath)
import Purebred.System.Process
import Purebred.Types.IFC (sanitiseText, untaint)
import Purebred.Types.Error

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

-- * Attributes
-- $attributes
-- These attributes are used as keys in widgets to assign color values.
--

-- ** State Attributes used to indicate list item state
-- List attributes are generated based on three possible states:
--
--   * selected (first)
--   * toggled
--   * new (last)
--
-- That means that for example, any new list items will inherit colour
-- definitions from selected and toggled attributes.
listStateSelectedAttr :: A.AttrName
listStateSelectedAttr = "selected"

listStateNewmailAttr :: A.AttrName
listStateNewmailAttr = "newmail"

listStateToggledAttr :: A.AttrName
listStateToggledAttr = "toggled"

-- ** Widget Attributes
--
defaultAttr :: A.AttrName
defaultAttr = "default"

mailViewAttr :: A.AttrName
mailViewAttr = "mailview"

statusbarAttr :: A.AttrName
statusbarAttr = "statusbar"

statusbarErrorAttr :: A.AttrName
statusbarErrorAttr = statusbarAttr <> "error"

statusbarInfoAttr :: A.AttrName
statusbarInfoAttr = statusbarAttr <> "info"

statusbarWarningAttr :: A.AttrName
statusbarWarningAttr = statusbarAttr <> "warning"

editorAttr :: A.AttrName
editorAttr = E.editAttr

editorFocusedAttr :: A.AttrName
editorFocusedAttr = E.editFocusedAttr

editorErrorAttr :: A.AttrName
editorErrorAttr = editorAttr <> "error"

editorLabelAttr :: A.AttrName
editorLabelAttr = editorAttr <> "label"

listAttr :: A.AttrName
listAttr = L.listAttr

-- Note: Brick exports a L.listSelectedAttr, yet in order to make our
-- use of our listState attributes consistent across the application
-- we need to use our own listState attributes.
listSelectedAttr :: A.AttrName
listSelectedAttr = L.listAttr <> listStateSelectedAttr

listNewMailAttr :: A.AttrName
listNewMailAttr = L.listAttr <> listStateNewmailAttr

listSelectedNewmailAttr :: A.AttrName
listSelectedNewmailAttr = L.listSelectedAttr <> listStateNewmailAttr

listToggledAttr :: A.AttrName
listToggledAttr = L.listAttr <> listStateToggledAttr

listSelectedToggledAttr :: A.AttrName
listSelectedToggledAttr = listStateSelectedAttr <> listToggledAttr

mailAttr :: A.AttrName
mailAttr = "mail"

mailTagAttr :: A.AttrName
mailTagAttr = mailAttr <> "tag"

mailTagToggledAttr :: A.AttrName
mailTagToggledAttr = mailTagAttr <> listStateToggledAttr

mailAuthorsAttr :: A.AttrName
mailAuthorsAttr = mailAttr <> "authors"

mailNewmailAuthorsAttr :: A.AttrName
mailNewmailAuthorsAttr = mailAuthorsAttr <> listStateNewmailAttr

mailToggledAuthorsAttr :: A.AttrName
mailToggledAuthorsAttr = mailAuthorsAttr <> listStateToggledAttr

mailSelectedAuthorsAttr :: A.AttrName
mailSelectedAuthorsAttr = mailAuthorsAttr <> listStateSelectedAttr

mailSelectedNewmailAuthorsAttr :: A.AttrName
mailSelectedNewmailAuthorsAttr = mailAuthorsAttr <> listStateSelectedAttr <> listStateNewmailAttr

mailSelectedToggledAuthorsAttr :: A.AttrName
mailSelectedToggledAuthorsAttr = mailSelectedAuthorsAttr <> listStateToggledAttr

headerAttr :: A.AttrName
headerAttr = "header"

headerKeyAttr :: A.AttrName
headerKeyAttr = headerAttr <> "key"

headerValueAttr :: A.AttrName
headerValueAttr = headerAttr <> "value"

helpAttr :: A.AttrName
helpAttr = "help"

helpTitleAttr :: A.AttrName
helpTitleAttr = helpAttr <> "title"

helpKeybindingAttr :: A.AttrName
helpKeybindingAttr = helpAttr <> "keybinding"


textMatchHighlightAttr :: A.AttrName
textMatchHighlightAttr = "match"

currentTextMatchHighlightAttr :: A.AttrName
currentTextMatchHighlightAttr = textMatchHighlightAttr <> "current"

mailbodyAttr :: A.AttrName
mailbodyAttr = "mailbody"

mailbodySourceAttr :: A.AttrName
mailbodySourceAttr = mailbodyAttr <> "source"


-- * Purebred's Configuration

-- | The default configuration used in Purebred.
--
defaultConfig :: UserConfiguration
defaultConfig =
    Configuration
    { _confTheme = solarizedDark
    , _confNotmuch = NotmuchSettings
      { _nmSearch = "tag:inbox"
      , _nmDatabase = getDatabasePath
      , _nmNewTag = "unread"
      , _nmDraftTag = "draft"
      , _nmSentTag = "sent"
      , _nmHasNewMailSearch = "tag:inbox and tag:unread"
      , _nmHasNewMailCheckDelay = Just (Seconds 3)
      }
    , _confEditor = fromMaybe "vi" <$> lookupEnv "EDITOR"
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
      , _fbHomePath = getHomeDirectory
      }
    , _confCharsets = defaultCharsets
    , _confPlugins = set pluginBuiltIn True <$>
        [ usePlugin Purebred.Plugin.UserAgent.plugin
        ]
    , _confExtra = ()
    }
