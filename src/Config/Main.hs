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
import Data.Map.Lazy (fromList)
import UI.ComposeEditor.Keybindings (composeEditorKeybindings)
import UI.Index.Keybindings
       (indexKeybindings, indexsearchKeybindings)
import UI.GatherHeaders.Keybindings (interactiveGatherHeadersKeybindings)
import UI.Mail.Keybindings (displayMailKeybindings)
import Types
       (UserConfiguration, Configuration(..), MailViewSettings(..),
        NotmuchSettings(..), Mode(..))
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
        , (headerValueAttr, fg V.brightCyan)]

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
      }
    , _confKeybindings = fromList
          [ (BrowseMail, indexKeybindings)
          , (SearchMail, indexsearchKeybindings)
          , (ViewMail, displayMailKeybindings)
          , (GatherHeaders, interactiveGatherHeadersKeybindings)
          , (ComposeEditor, composeEditorKeybindings)]
    }
