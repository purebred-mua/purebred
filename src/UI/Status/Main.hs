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
import Config.Main (statusbarAttr, statusbarErrorAttr)

statusbar :: AppState -> Widget Name
statusbar s =
    case view asError s of
        Just e -> withAttr statusbarErrorAttr $ strWrap (show e)
        Nothing -> case view asAppMode s of
          BrowseMail -> renderStatusbar BrowseMail (view (asMailIndex . miListOfMails) s)
          m -> renderStatusbar m (view (asMailIndex . miListOfThreads) s)

renderStatusbar :: Mode -> L.List Name e -> Widget Name
renderStatusbar m l =
  let mode = str $ show $ m
      total = str $ show $ length $ view L.listElementsL l
  in withAttr statusbarAttr $ str "Purebred: " <+>
               str "Item " <+> currentIndexW l <+> str " of " <+> total <+> fillLine <+> mode

currentIndexW :: L.List Name e -> Widget Name
currentIndexW l = str $ show $ currentIndex l

currentIndex :: L.List Name e -> Int
currentIndex l = fromMaybe 0 $ view L.listSelectedL l
