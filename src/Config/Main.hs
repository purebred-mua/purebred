{-# LANGUAGE OverloadedStrings #-}

module Config.Main where

import qualified Brick.AttrMap as A
import qualified Brick.Widgets.List as L
import Brick.Themes (Theme, newTheme)
import Data.Monoid ((<>))
import Brick.Util (fg, on, bg)
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
import System.Environment (lookupEnv)
import System.Directory (getHomeDirectory)
import Data.Maybe (fromMaybe)
import Network.Mail.Mime (renderSendMail)

import Data.MIME (ContentType(..))

import UI.FileBrowser.Keybindings
       (fileBrowserKeybindings, manageSearchPathKeybindings)
import UI.GatherHeaders.Keybindings
       (gatherFromKeybindings,
        gatherToKeybindings,
        gatherSubjectKeybindings)
import UI.Index.Keybindings
       (browseMailKeybindings, browseThreadsKeybindings,
        searchThreadsKeybindings, manageThreadTagsKeybindings,
        manageMailTagsKeybindings)
import UI.Mail.Keybindings
       (displayMailKeybindings, mailViewManageMailTagsKeybindings)
import UI.Help.Keybindings (helpKeybindings)
import UI.ComposeEditor.Keybindings
       (listOfAttachmentsKeybindings, composeFromKeybindings,
        composeToKeybindings, composeSubjectKeybindings)

import Types
import Storage.Notmuch (getDatabasePath)

solarizedDark :: Theme
solarizedDark =
    newTheme
        V.defAttr
        [ (listAttr, V.brightBlue `on` V.brightBlack)
        , (listSelectedAttr, V.white `on` V.yellow)
        , (listNewMailAttr, fg V.white `V.withStyle` V.bold)
        , (listNewMailSelectedAttr, V.white `on` V.yellow `V.withStyle` V.bold)
        , (mailTagsAttr, fg V.cyan)
        , (mailAuthorsAttr, fg V.white)
        , (E.editFocusedAttr, V.white `on` V.brightBlack)
        , (editorAttr, V.brightBlue `on` V.brightBlack)
        , (editorLabelAttr, V.brightYellow `on` V.brightBlack)
        , (statusbarErrorAttr, fg V.red)
        , (statusbarAttr, V.brightYellow `on` V.black)
        , (headerKeyAttr, fg V.cyan)
        , (headerValueAttr, fg V.brightCyan)
        , (helpTitleAttr, fg V.cyan `V.withStyle` V.bold)]

solarizedLight :: Theme
solarizedLight =
    newTheme
        V.defAttr
        [ (listAttr, V.brightCyan `on` V.brightWhite)
        , (listSelectedAttr, V.white `on` V.yellow)
        , (listNewMailAttr, fg V.brightGreen `V.withStyle` V.bold)
        , (listNewMailSelectedAttr, V.white `on` V.yellow `V.withStyle` V.bold)
        , (mailTagsAttr, fg V.magenta)
        , (mailAuthorsAttr, fg V.brightCyan)
        , (mailSelectedAuthorsAttr, fg V.brightWhite)
        , (E.editFocusedAttr, V.brightBlack `on` V.brightWhite)
        , (editorAttr, V.brightBlue `on` V.brightWhite)
        , (editorLabelAttr, V.brightYellow `on` V.brightWhite)
        , (statusbarErrorAttr, fg V.red)
        , (statusbarAttr, V.brightYellow `on` V.white)
        , (mailViewAttr, bg V.brightWhite)
        , (headerKeyAttr, V.cyan `on` V.brightWhite)
        , (headerValueAttr, V.brightCyan `on` V.brightWhite)
        , (helpTitleAttr, fg V.cyan `V.withStyle` V.bold)]

mailViewAttr :: A.AttrName
mailViewAttr = "mailview"

statusbarAttr :: A.AttrName
statusbarAttr = "statusbar"

statusbarErrorAttr :: A.AttrName
statusbarErrorAttr = statusbarAttr <> "error"

editorAttr :: A.AttrName
editorAttr = E.editAttr

editorFocusedAttr :: A.AttrName
editorFocusedAttr = E.editFocusedAttr

editorLabelAttr :: A.AttrName
editorLabelAttr = editorAttr <> "label"

listAttr :: A.AttrName
listAttr = L.listAttr

listSelectedAttr :: A.AttrName
listSelectedAttr = L.listSelectedAttr

listNewMailAttr :: A.AttrName
listNewMailAttr = L.listAttr <> "newmail"

listNewMailSelectedAttr :: A.AttrName
listNewMailSelectedAttr = listNewMailAttr <> L.listSelectedAttr

mailAttr :: A.AttrName
mailAttr = "mail"

mailTagsAttr :: A.AttrName
mailTagsAttr = mailAttr <> "tags"

mailAuthorsAttr :: A.AttrName
mailAuthorsAttr = mailAttr <> "authors"

mailSelectedAuthorsAttr :: A.AttrName
mailSelectedAuthorsAttr = mailAuthorsAttr <> "selected"

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

defaultConfig :: UserConfiguration
defaultConfig =
    Configuration
    { _confTheme = solarizedDark
    , _confNotmuch = NotmuchSettings
      { _nmSearch = "tag:inbox"
      , _nmDatabase = getDatabasePath
      , _nmNewTag = "unread"
      }
    , _confEditor = fromMaybe "vi" <$> lookupEnv "EDITOR"
    , _confMailView = MailViewSettings
      { _mvIndexRows = 10
      , _mvPreferredContentType = ContentType "text" "plain" []
      , _mvHeadersToShow = (`elem` ["subject", "to", "from", "cc"])
      , _mvKeybindings = displayMailKeybindings
      , _mvManageMailTagsKeybindings = mailViewManageMailTagsKeybindings
      }
    , _confIndexView = IndexViewSettings
      { _ivBrowseThreadsKeybindings = browseThreadsKeybindings
      , _ivBrowseMailsKeybindings = browseMailKeybindings
      , _ivSearchThreadsKeybindings = searchThreadsKeybindings
      , _ivManageMailTagsKeybindings = manageMailTagsKeybindings
      , _ivManageThreadTagsKeybindings = manageThreadTagsKeybindings
      , _ivFromKeybindings = gatherFromKeybindings
      , _ivToKeybindings = gatherToKeybindings
      , _ivSubjectKeybindings = gatherSubjectKeybindings
      }
    , _confComposeView = ComposeViewSettings
      { _cvFromKeybindings = composeFromKeybindings
      , _cvToKeybindings = composeToKeybindings
      , _cvSubjectKeybindings = composeSubjectKeybindings
      , _cvSendMailCmd = renderSendMail
      , _cvListOfAttachmentsKeybindings = listOfAttachmentsKeybindings
      , _cvBoundary = []
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
    }
