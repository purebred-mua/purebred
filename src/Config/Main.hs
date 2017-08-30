{-# LANGUAGE OverloadedStrings #-}
module Config.Main where

import qualified Brick.AttrMap as A
import Brick.Util (fg, on)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V
import UI.Mail.Main (headerKeyAttr, headerValueAttr)
import UI.ComposeEditor.Keybindings (composeEditorKeybindings)
import UI.Index.Main
       (listAttr, listSelectedAttr, listNewMailAttr, mailTagsAttr)
import UI.Status.Main (statusbarAttr, statusbarErrorAttr)
import UI.Index.Keybindings
       (indexKeybindings, indexsearchKeybindings)
import UI.Mail.Keybindings (displayMailKeybindings)
import Types
  ( ComposeViewSettings(..)
  , UserConfiguration, Configuration(..), IndexViewSettings(..)
  , MailViewSettings(..), NotmuchSettings(..)
  )
import Storage.Notmuch (getDatabasePath)

defaultColorMap :: A.AttrMap
defaultColorMap =
    A.attrMap
        V.defAttr
        [ (listAttr, V.brightBlue `on` V.black)
        , (listSelectedAttr, V.white `on` V.yellow)
        , (listNewMailAttr, fg V.white `V.withStyle` V.bold)
        , (mailTagsAttr, fg V.cyan)
        , (E.editFocusedAttr, V.white `on` V.black)
        , (E.editAttr, V.brightBlue `on` V.black)
        , (statusbarErrorAttr, fg V.red)
        , (statusbarAttr, V.black `on` V.brightWhite)
        , (headerKeyAttr, fg V.cyan)
        , (headerValueAttr, fg V.brightCyan)]

defaultConfig :: UserConfiguration
defaultConfig =
    Configuration
    { _confColorMap = defaultColorMap
    , _confNotmuch = NotmuchSettings
      { _nmSearch = "tag:inbox"
      , _nmDatabase = getDatabasePath
      , _nmNewTag = "unread"
      }
    , _confEditor = "vi"
    , _confMailView = MailViewSettings
      { _mvIndexRows = 10
      , _mvPreferedContentType = "text/plain"
      , _mvHeadersToShow = (`elem` ["subject", "to", "from"])
      , _mvKeybindings = displayMailKeybindings
      }
    , _confIndexView = IndexViewSettings
      { _ivKeybindings = indexKeybindings
      , _ivSearchKeybindings = indexsearchKeybindings
      }
    , _confComposeView = ComposeViewSettings
      { _cvKeybindings = composeEditorKeybindings
      }
    }
