{-# LANGUAGE OverloadedStrings #-}
module UI.App where

import           Storage.Mail       (Mail)
import           UI.Draw.Mail       (drawMail)
import           UI.Draw.Main       (drawMain, theMap)
import           UI.Event.Mail      (mailEvent)
import           UI.Event.Main      (mainEvent)
import           UI.Types

import           Lens.Micro         ((^.))


import qualified Brick.Main         as M
import           Brick.Types        (Widget)
import qualified Brick.Types        as T
import qualified Brick.Widgets.List as L
import qualified Data.Vector        as Vec

drawUI :: AppState -> [Widget ()]
drawUI s =
    case s ^. appMode of
        Main -> drawMain s
        ViewMail -> drawMail s

appEvent :: AppState -> T.BrickEvent () e -> T.EventM () (T.Next AppState)
appEvent s e =
    case s ^. appMode of
        Main -> mainEvent s e
        ViewMail -> mailEvent s e

initialState :: Vec.Vector Mail -> AppState
initialState vec = AppState "tag:inbox" (L.list () vec 1) Main

theApp :: M.App AppState e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }
