{-# LANGUAGE OverloadedStrings #-}

module Config.Main where

import qualified Brick.AttrMap as A
import qualified Brick.Widgets.List as L
import Data.Monoid ((<>))
import Brick.Util (fg, on)
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import UI.ComposeEditor.Keybindings (composeEditorKeybindings)
import UI.Index.Keybindings
       (indexKeybindings, indexsearchKeybindings)
import UI.Mail.Keybindings (displayMailKeybindings, displayIndexKeybindings)
import UI.Help.Keybindings (helpKeybindings)
import Types
import Storage.Notmuch (getDatabasePath)

defaultColorMap :: A.AttrMap
defaultColorMap =
    A.attrMap
        V.defAttr
        [ (listAttr, V.brightBlue `on` V.black)
        , (listSelectedAttr, V.white `on` V.yellow)
        , (listNewMailAttr, fg V.white `V.withStyle` V.bold)
        , (listNewMailSelectedAttr, V.white `on` V.yellow `V.withStyle` V.bold)
        , (mailTagsAttr, fg V.cyan)
        , (E.editFocusedAttr, V.white `on` V.black)
        , (E.editAttr, V.brightBlue `on` V.black)
        , (statusbarErrorAttr, fg V.red)
        , (statusbarAttr, V.black `on` V.brightWhite)
        , (headerKeyAttr, fg V.cyan)
        , (headerValueAttr, fg V.brightCyan)
        , (helpTitleAttr, fg V.cyan `V.withStyle` V.bold)]

statusbarAttr :: A.AttrName
statusbarAttr = "statusbar"

statusbarErrorAttr :: A.AttrName
statusbarErrorAttr = statusbarAttr <> "error"

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
    { _confColorMap = defaultColorMap
    , _confNotmuch = NotmuchSettings
      { _nmSearch = "tag:inbox"
      , _nmDatabase = getDatabasePath
      , _nmNewTag = "unread"
      }
    , _confEditor = fromMaybe "vi" <$> lookupEnv "EDITOR"
    , _confMailView = MailViewSettings
      { _mvIndexRows = 10
      , _mvPreferedContentType = "text/plain"
      , _mvHeadersToShow = (`elem` ["subject", "to", "from"])
      , _mvKeybindings = displayMailKeybindings
      , _mvIndexKeybindings = displayIndexKeybindings
      }
    , _confIndexView = IndexViewSettings
      { _ivKeybindings = indexKeybindings
      , _ivSearchKeybindings = indexsearchKeybindings
      }
    , _confComposeView = ComposeViewSettings
      { _cvKeybindings = composeEditorKeybindings
      }
    , _confHelpView = HelpViewSettings
      { _hvKeybindings = helpKeybindings
      }
    }
