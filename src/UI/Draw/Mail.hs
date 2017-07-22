module UI.Draw.Mail where

import           Brick.Types        (Widget)
import           Brick.Widgets.Core (str, (<=>), (<+>))
import qualified Brick.Widgets.List as L
import           Data.Maybe         (fromMaybe)
import qualified Data.Vector        as Vec
import           Lens.Micro         ((^.))
import           Storage.Mail
import           UI.Draw.Main       (listDrawElement)
import           UI.Types

-- | TODO: This should draw part of the mail list in the top area, the actual
--   mail the size 3/4 of the viewing area with headers at the top and the mail body
drawMail :: AppState -> [Widget ()]
drawMail s =
    case L.listSelectedElement (s^.mailIndex) of
        Just (_, m) -> [indexView s <=> mailView m]
        Nothing -> [str "Eeek"]

mailView :: Mail -> Widget ()
mailView m = let widgets = zipWith (<+>) [str "From: ", str "To: ", str "Subject: "] [str (m^.from), str (m^.to), str (m^.subject)]
             in foldr (<=>) (str "") widgets

indexView :: AppState -> Widget ()
indexView s = L.renderList listDrawElement True sliced
  where sliced = slicedIndex $ s^.mailIndex

-- | In order to show part of the index, determine the lower and upper
--   boundaries for the index so that it always stays in view while keep the
--   current selected element selected
determineIndexBounds :: L.List () Mail -> (Int, Int)
determineIndexBounds l =
    let cur = fromMaybe 0 $ l^.L.listSelectedL
        slice_length = 7
        slice_start = max (cur - 6) 0
    in (slice_start, slice_length)

slicedIndex :: L.List () Mail -> L.List () Mail
slicedIndex l =
    let items = l ^. L.listElementsL
        (left,len) = determineIndexBounds l
        newlist = Vec.slice left len items
    in L.list () newlist 6
