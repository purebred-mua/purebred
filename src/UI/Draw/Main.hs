{-# LANGUAGE OverloadedStrings #-}
module UI.Draw.Main where

import qualified Brick.AttrMap      as A
import           Brick.Types        (Widget)
import           Brick.Util         (fg, on)
import           Brick.Widgets.Core (str, vBox, withAttr, (<+>))
import qualified Brick.Widgets.List as L
import           Data.Monoid        ((<>))
import qualified Data.Vector        as Vec
import qualified Graphics.Vty       as V
import           Lens.Micro         ((^.))
import           UI.Types

drawMain :: AppState -> [Widget ()]
drawMain s = [ui]
  where
    label =
        str "Purebred: " <+>
        str "Item " <+>
        cur <+>
        str " of " <+>
        total <+> str " search: " <+> str (s ^. notmuchRawsearch)
    l = s ^. mailIndex
    cur =
        case l ^. (L.listSelectedL) of
            Nothing -> str "-"
            Just i -> str (show (i + 1))
    total = str $ show $ Vec.length $ l ^. (L.listElementsL)
    box = L.renderList listDrawElement True l
    ui = vBox [box, label]

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
    let selStr s =
            if sel
                then withAttr customAttr (str $ "<" <> s <> ">")
                else str s
    in (selStr $ show a)

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    ]
