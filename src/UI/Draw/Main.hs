{-# LANGUAGE OverloadedStrings #-}
module UI.Draw.Main where

import qualified Brick.AttrMap      as A
import           Brick.Types        (Padding (..), Widget)
import           Brick.Util         (fg, on)
import           Brick.Widgets.Core (hLimit, padLeft, str, vBox, withAttr, (<+>))
import qualified Brick.Widgets.List as L
import           Data.Maybe         (fromMaybe)
import           Data.Monoid        ((<>))
import qualified Data.Vector        as Vec
import qualified Graphics.Vty       as V
import           Lens.Micro         ((^.))
import           Storage.Mail
import           UI.Types

drawMain :: AppState -> [Widget ()]
drawMain s = [ui]
  where
    label =
        str "Purebred: " <+>
        str "Item " <+>
        currentIndexW l <+>
        str " of " <+>
        total <+> str " search: " <+> str (s ^. notmuchRawsearch)
    l = s ^. mailIndex
    total = str $ show $ Vec.length $ l ^. (L.listElementsL)
    box = L.renderList listDrawElement True l
    ui = vBox [box, label]

listDrawElement :: Bool -> Mail -> Widget ()
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
    , (customAttr,            fg V.cyan)
    ]

currentIndexW :: L.List () Mail -> Widget ()
currentIndexW l = str $ show $ currentIndex l

currentIndex :: L.List () Mail -> Int
currentIndex l = fromMaybe 0 $ l^.L.listSelectedL
