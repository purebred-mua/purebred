{-# LANGUAGE OverloadedStrings #-}
module UI.App where

import Storage.Notmuch (getMessages)
import Storage.Mail (Mail(..))

import qualified Notmuch as NM

import Lens.Micro ((^.))

import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO)

import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import qualified Brick.AttrMap as A
import qualified Data.Vector as Vec
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>)
  , str
  , vBox
  , withAttr
  )
import Brick.Util (fg, on)

data Mode
    = Main
    | ViewMail

data AppState = AppState
    { search :: String
    , index :: L.List () Mail
    , mode :: Mode
    }

drawUI :: AppState -> [Widget ()]
drawUI (AppState s l Main) = [ui]
    where
        label =  str "Purebred: " <+> str "Item " <+> cur <+> str " of " <+> total <+> str " search: " <+> str s
        cur = case l^.(L.listSelectedL) of
                Nothing -> str "-"
                Just i -> str (show (i + 1))
        total = str $ show $ Vec.length $ l^.(L.listElementsL)
        box = L.renderList listDrawElement True l
        ui = vBox [ box, label ]
drawUI (AppState _ l ViewMail) =
    case L.listSelectedElement l of
        Just (_, m) -> [vBox [str (subject m), str (from m), str (to m)]]
        Nothing -> [str "Eeek"]

appEvent :: AppState -> T.BrickEvent () e -> T.EventM () (T.Next AppState)
appEvent a@(AppState s l Main) (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> M.halt a
        V.EvKey V.KEnter [] -> M.continue $ AppState s l ViewMail
        ev -> L.handleListEvent ev l >>= \l' -> M.continue $ AppState s l' Main
appEvent (AppState s l ViewMail) (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.continue $ AppState s l Main
    _ -> M.continue $ AppState s l ViewMail
appEvent l _ = M.continue l

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in (selStr $ show a)

initialState :: Vec.Vector Mail -> AppState
initialState vec = AppState "tag:inbox" (L.list () vec 1) Main

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    ]

theApp :: M.App AppState e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }
