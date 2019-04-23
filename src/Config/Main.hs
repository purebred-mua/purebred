{-# LANGUAGE OverloadedStrings #-}

module Config.Main where

import qualified Brick.AttrMap as A
import qualified Brick.Widgets.List as L
import Data.Monoid ((<>))
import Brick.Util (fg, on, bg)
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.Directory (getHomeDirectory)
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (fromList)
import System.Exit (ExitCode(..))

import Data.MIME (contentTypeTextPlain, matchContentType)

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
       (displayMailKeybindings, mailViewManageMailTagsKeybindings,
        mailAttachmentsKeybindings, openWithKeybindings,
        pipeToKeybindings)
import UI.Help.Keybindings (helpKeybindings)
import UI.ComposeEditor.Keybindings
       (listOfAttachmentsKeybindings, composeFromKeybindings,
        composeToKeybindings, composeSubjectKeybindings)

import Error
import Types
import Purebred.System.Process
import Purebred.Types.IFC (sanitiseText, untaint)
import Storage.Notmuch (getDatabasePath)

sendmailPath :: FilePath
sendmailPath = "/usr/sbin/sendmail"

renderSendMail :: FilePath -> B.ByteString -> IO (Either Error ())
renderSendMail sendmail m = do
  -- -t which extracts recipients from the mail
  result <- tryRunProcess config
  pure $ case result of
    Left e -> Left $ SendMailError (show e)
    Right (ExitFailure _, stderr) -> Left $ SendMailError (untaint decode stderr)
    Right (ExitSuccess, _) -> Right ()
  where
    config = setStdin (byteStringInput (LB.fromStrict m)) $ proc sendmail ["-t", "-v"]
    decode = T.unpack . sanitiseText . decodeLenient . LB.toStrict

solarizedDark :: A.AttrMap
solarizedDark =
    A.attrMap
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
        , (statusbarAttr, V.black `on` V.brightYellow)
        , (headerKeyAttr, fg V.cyan)
        , (headerValueAttr, fg V.brightCyan)
        , (helpTitleAttr, fg V.cyan `V.withStyle` V.bold)]

solarizedLight :: A.AttrMap
solarizedLight =
    A.attrMap
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
      , _mvPreferredContentType = contentTypeTextPlain
      , _mvHeadersToShow = (`elem` ["subject", "to", "from", "cc", "date"])
      , _mvKeybindings = displayMailKeybindings
      , _mvManageMailTagsKeybindings = mailViewManageMailTagsKeybindings
      , _mvMailListOfAttachmentsKeybindings = mailAttachmentsKeybindings
      , _mvOpenWithKeybindings = openWithKeybindings
      , _mvPipeToKeybindings = pipeToKeybindings
      , _mvMailcap =
          [ ( matchContentType "text" (Just "html")
            , MailcapHandler (Shell (fromList "elinks -force-html")) False)
          , ( const True
            , MailcapHandler (Process (fromList "xdg-open") []) True)
          ]
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
      , _cvSendMailPath = sendmailPath
      , _cvListOfAttachmentsKeybindings = listOfAttachmentsKeybindings
      , _cvIdentities = []
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
    , _confExtra = ()
    }
