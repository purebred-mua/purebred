{-# LANGUAGE OverloadedStrings #-}
module Config.Main where

import qualified Brick.AttrMap as A
import Brick.Util (fg, on)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V
import UI.ComposeEditor.Keybindings (composeEditorKeybindings)
import UI.Index.Main (listNewMailAttr, mailTagsAttr)
import UI.Index.Keybindings
       (indexKeybindings, indexsearchKeybindings)
import UI.Mail.Keybindings (displayMailKeybindings)
import Types
       (ComposeViewSettings(..), Configuration(..), IndexViewSettings(..),
        MailViewSettings(..), NotmuchSettings(..))

defaultColorMap :: A.AttrMap
defaultColorMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.brightBlue `on` V.black)
    , (L.listSelectedAttr,    V.white `on` V.yellow)
    , (listNewMailAttr,       fg V.white `V.withStyle` V.bold)
    , (mailTagsAttr,              fg V.cyan)
    , (E.editFocusedAttr,     V.white `on` V.black)
    , (E.editAttr,            V.brightBlue `on` V.black)
    , (A.attrName "error",    fg V.red)
    , (A.attrName "statusbar", V.black `on` V.brightWhite)
    ]

defaultConfig :: String -> IO Configuration
defaultConfig dbfp =
    pure
        Configuration
        { _confColorMap = defaultColorMap
        , _confNotmuch = NotmuchSettings
          { _nmSearch = "tag:inbox"
          , _nmDatabase = dbfp
          , _nmNewTag = "unread"
          }
        , _confEditor = "vi"
        , _confMailView = MailViewSettings
          { _mvIndexRows = 10
          , _mvPreferedContentType = "text/plain"
          , _mvHeadersToShow = ["subject", "to", "from"]
          , _mvKeybindings = displayMailKeybindings
          }
        , _confIndexView = IndexViewSettings
          { _ivKeybindings = indexKeybindings
          , _ivSearchKeybindings = indexsearchKeybindings
          }
        , _confComposeView = ComposeViewSettings
        {
          _cvKeybindings = composeEditorKeybindings
        }
        }
