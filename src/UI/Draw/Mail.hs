module UI.Draw.Mail where

import           Brick.Types        (Widget)
import           Brick.Widgets.Core (str, vBox)
import qualified Brick.Widgets.List as L
import           Lens.Micro         ((^.))
import           Storage.Mail
import           UI.Types

-- | TODO: This should draw part of the mail list in the top area, the actual
--   mail the size 3/4 of the viewing area with headers at the top and the mail body
drawMail :: AppState -> [Widget ()]
drawMail s =
    case L.listSelectedElement (s^.mailIndex) of
        Just (_,m) -> [vBox [str (m^.subject), str (m^.from), str (m^.to)]]
        Nothing -> [str "Eeek"]
