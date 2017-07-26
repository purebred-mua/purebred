-- | module for drawing main window widgets
{-# LANGUAGE OverloadedStrings #-}
module UI.Draw.Main where

import qualified Brick.AttrMap      as A
import           Brick.Types        (Padding (..), Widget)
import           Brick.Util         (fg, on)
import           Brick.Widgets.Core (hLimit, padLeft, str, txt, vBox, vLimit,
                                     withAttr, (<+>))
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import           Data.Maybe         (fromMaybe)
import           Data.Monoid        ((<>))
import qualified Data.Text          as T
import qualified Data.Vector        as Vec
import qualified Graphics.Vty       as V
import           Lens.Micro         ((^.))
import           Storage.Mail
import           UI.Types

drawMain :: AppState -> [Widget Name]
drawMain s = [ui]
  where
    label =
        str "Purebred: " <+>
        str "Item " <+> currentIndexW l <+> str " of " <+> total
    editorFocus = case (s^.mailIndex^.miMode) of
      BrowseMail -> False
      SearchMail -> True
    inputBox = E.renderEditor editorFocus (s ^. mailIndex ^. searchEditor)
    l = s ^. mailIndex ^. listOfMails
    total = str $ show $ Vec.length $ l ^. (L.listElementsL)
    box = L.renderList listDrawElement False l
    ui = vBox [box, label, vLimit 1 inputBox]

editorDrawContent :: [T.Text] -> Widget Name
editorDrawContent st = txt $ T.unlines st

listDrawElement :: Bool -> Mail -> Widget Name
listDrawElement sel a =
    let selStr w =
            if sel
                then withAttr customAttr w
                else w
    in (selStr $
        padLeft (Pad 1) $
        hLimit 15 (str $ a ^. from) <+> padLeft (Pad 2) (str (a ^. subject)))

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (E.editFocusedAttr,     V.white `on` V.black)
    , (E.editAttr,            V.black `on` V.white)
    , (customAttr,            fg V.cyan)
    ]

currentIndexW :: L.List Name Mail -> Widget Name
currentIndexW l = str $ show $ currentIndex l

currentIndex :: L.List Name Mail -> Int
currentIndex l = fromMaybe 0 $ l^.L.listSelectedL
