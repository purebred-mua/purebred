{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative hiding (str)

import Notmuch
import Notmuch.Search

import Lens.Micro ((^.))
import Control.Monad (void)

import Data.Monoid
import Data.Foldable (toList)
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

drawUI :: (Show a) => L.List () a -> [Widget ()]
drawUI l = [ui]
    where
        label =  str "Purebred: " <+> str "Item " <+> cur <+> str " of " <+> total
        cur = case l^.(L.listSelectedL) of
                Nothing -> str "-"
                Just i -> str (show (i + 1))
        total = str $ show $ Vec.length $ l^.(L.listElementsL)
        box = L.renderList listDrawElement True l
        ui = vBox [ box, label ]

appEvent :: L.List () String -> T.BrickEvent () e -> T.EventM () (T.Next (L.List () String))
appEvent l (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> M.halt l
        ev -> M.continue =<< L.handleListEvent ev l
appEvent l _ = M.continue l

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in (selStr $ show a)

initialState :: Vec.Vector String -> L.List () String
initialState vec = L.list () vec 1

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    ]

theApp :: M.App (L.List () String) e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

data AppConfig = AppConfig { databaseFilepath :: String }

appconfig :: Parser AppConfig
appconfig = AppConfig <$> strOption ( long "database" <> metavar "DATABASE" <> help "Filepath to notmuch database" )

main :: IO ()
main = do
    msgs <- getMessages =<< execParser opts
    void $ M.defaultMain theApp (initialState msgs)
        where
            opts = info (appconfig <**> helper)
                ( fullDesc
                <> progDesc "purebred"
                <> header "a search based, terminal mail user agent")

-- Notmuch
getMessages :: AppConfig -> IO (Vec.Vector String)
getMessages config = do
  db' <- databaseOpen (databaseFilepath config)
  case db' of
    Left status -> do
        error $ show status
    Right db -> do
        q <- query db (FreeForm "tag:inbox")
        msgs <- messages q
        hdrs <- (mapM (messageHeader "Subject")) msgs
        return $ Vec.fromList $ toList hdrs
