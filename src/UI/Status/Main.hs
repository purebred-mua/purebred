{-# LANGUAGE OverloadedStrings #-}

module UI.Status.Main where

import Brick.Types (Widget)
import Brick.Widgets.Core (str, withAttr, (<+>), strWrap)
import qualified Brick.Widgets.List  as L
import Control.Lens.Getter (view)
import Data.Maybe (fromMaybe)
import Data.Vector (length)
import Prelude hiding (length)
import UI.Draw.Main (fillLine)
import Types
       (NotmuchMail, AppState, Name, asError, asMailIndex, miListOfMails,
        asAppMode)
import Config.Main (statusbarAttr, statusbarErrorAttr)

statusbar :: AppState -> Widget Name
statusbar s =
    case view asError s of
        Just e -> withAttr statusbarErrorAttr $ strWrap (show e)
        Nothing ->
            let l = view (asMailIndex . miListOfMails) s
                total = str $ show $ length $ view L.listElementsL l
                mode = str $ show $ view asAppMode s
            in withAttr statusbarAttr $ str "Purebred: " <+>
               str "Item " <+> currentIndexW l <+> str " of " <+> total <+> fillLine <+> mode

currentIndexW :: L.List Name NotmuchMail -> Widget Name
currentIndexW l = str $ show $ currentIndex l

currentIndex :: L.List Name NotmuchMail -> Int
currentIndex l = fromMaybe 0 $ view L.listSelectedL l
