{-# LANGUAGE OverloadedStrings #-}

module UI.Status.Main where

import Brick.Types (Widget)
import Brick.Widgets.Core (str, withAttr, (<+>), strWrap)
import qualified Brick.Widgets.List  as L
import Control.Lens.Getter (view)
import Data.Semigroup ((<>))
import UI.Draw.Main (fillLine)
import Types
import Config.Main (statusbarAttr, statusbarErrorAttr)

statusbar :: AppState -> Widget Name
statusbar s =
    case view asError s of
        Just e -> withAttr statusbarErrorAttr $ strWrap (show e)
        Nothing -> case view asAppMode s of
          BrowseMail -> renderStatusbar BrowseMail (view (asMailIndex . miListOfMails) s)
          ViewMail -> renderStatusbar ViewMail (view (asMailIndex . miListOfMails) s)
          m -> renderStatusbar m (view (asMailIndex . miListOfThreads) s)

renderStatusbar :: Mode -> L.List Name e -> Widget Name
renderStatusbar m l =
  withAttr statusbarAttr $
    str "Purebred: " <+> currentItemW l <+> fillLine <+> str (show m)

currentItemW :: L.List Name e -> Widget Name
currentItemW l = str $
  maybe
    "No items"
    (\i -> "Item " <> show (i + 1) <> " of " <> total)
    (view L.listSelectedL l)
  where
      total = show $ length $ view L.listElementsL l
