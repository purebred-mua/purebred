{-# LANGUAGE OverloadedStrings #-}
module UI.Status.Main where

import           Brick.Types         (Widget)
import           Brick.Widgets.Core  (str, withAttr, (<+>))
import qualified Brick.Widgets.List  as L
import           Control.Lens.Getter ((^.))
import           Data.Maybe          (fromMaybe)
import           Data.Vector         (length)
import           Prelude             hiding (length)
import           Storage.Mail        (Mail)
import           UI.Draw.Main        (fillLine)
import Types
       (AppState, Name, asError, asMailIndex, miListOfMails)

statusbar :: AppState -> Widget Name
statusbar s =
    case s ^. asError of
        Just e -> withAttr "error" $ str e
        Nothing ->
            let l = s ^. asMailIndex ^. miListOfMails
                total = str $ show $ length $ l ^. (L.listElementsL)
            in withAttr "statusbar" $ str "Purebred: " <+>
               str "Item " <+> currentIndexW l <+> str " of " <+> total <+> fillLine

currentIndexW :: L.List Name Mail -> Widget Name
currentIndexW l = str $ show $ currentIndex l

currentIndex :: L.List Name Mail -> Int
currentIndex l = fromMaybe 0 $ l^.L.listSelectedL
