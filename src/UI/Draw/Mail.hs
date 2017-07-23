module UI.Draw.Mail where

import           Brick.Types        (Widget)
import           Brick.Widgets.Core (str, (<=>), (<+>), vLimit)
import qualified Brick.Widgets.List as L
import           Data.Maybe         (fromMaybe)
import qualified Data.Vector        as Vec
import           Lens.Micro         ((^.))
import           Storage.Mail
import           UI.Draw.Main       (listDrawElement)
import           UI.Types

-- | Instead of using the entire rendering area to show the email, we still show
-- the index in context above the mail.
--
-- Implementation detail: Currently we're creating the sub list of mails we show
-- for each key press. This might have to change in the future.
drawMail :: AppState -> [Widget ()]
drawMail s =
    case L.listSelectedElement (s^.mailIndex) of
        Just (_, m) -> [indexView s <=> mailView m]
        Nothing -> [str "Eeek"]

-- | TODO: See #19
mailView :: Mail -> Widget ()
mailView m =
    let widgets =
            zipWith
                (<+>)
                [str "From: ", str "To: ", str "Subject: "]
                [str (m ^. from), str (m ^. to), str (m ^. subject)]
    in foldr (<=>) (str "") widgets

indexView :: AppState -> Widget ()
indexView s = L.renderList listDrawElement True sliced
  where sliced = slicedIndex $ s^.mailIndex

-- | The size limit of the index list
indexViewRows :: Int
indexViewRows = 10

-- | In order to show part of the index, determine the lower and upper
--   boundaries for the index so that it always stays in view while keep the
--   current selected element selected
-- Workaround: Currently new list widgets position the selection at the first
-- item. So either we hack brick to allow setting the selected element (probably
-- best) or reduce the amount of elements we slice. The latter is implemented.
determineIndexBounds :: L.List () Mail -> (Int, Int)
determineIndexBounds l =
    let cur = fromMaybe 0 $ l^.L.listSelectedL
        total = Vec.length $ l^.L.listElementsL
        slice_length = if indexViewRows + cur > total then total - cur else indexViewRows
        slice_start = cur
    in (slice_start, slice_length)

slicedIndex :: L.List () Mail -> L.List () Mail
slicedIndex l =
    let items = l ^. L.listElementsL
        (left, len) = determineIndexBounds l
        newlist = Vec.slice left len items
    in L.list () newlist indexViewRows
