{-# LANGUAGE OverloadedStrings #-}
module Config.Main where

import qualified Brick.AttrMap        as A
import           Brick.Util           (fg, on)
import qualified Brick.Widgets.Edit   as E
import qualified Brick.Widgets.List   as L
import qualified Graphics.Vty         as V
import           UI.Index.Keybindings (indexKeybindings)
import           UI.Types             (Configuration (..), IndexView (..),
                                       MailView (..))

defaultColorMap :: A.AttrMap
defaultColorMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.brightBlue `on` V.black)
    , (L.listSelectedAttr,    V.white `on` V.yellow)
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
        , _confNotmuchsearch = "tag:inbox"
        , _confNotmuchDatabase = dbfp
        , _confEditor = "vi"
        , _confMailView = MailView
          { _mvIndexRows = 10
          , _mvPreferedContentType = "text/plain"
          , _mvHeadersToShow = ["subject", "to", "from"]
          }
        , _confIndexView = IndexView
          { _ivKeybindings = indexKeybindings
          }
        }
