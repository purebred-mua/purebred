-- | module for drawing main window widgets
{-# LANGUAGE OverloadedStrings #-}
module UI.Draw.Main where

import           Brick.Types         (Padding (..), Widget)
import           Brick.Widgets.Core  (hLimit, padLeft, str, txt, vBox, vLimit, fill,
                                      withAttr, (<+>))
import qualified Brick.Widgets.Edit  as E
import qualified Brick.Widgets.List  as L
import           Control.Lens.Getter ((^.))
import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T
import qualified Data.Vector         as Vec
import           Storage.Mail
import           UI.Types

drawMain :: AppState -> [Widget Name]
drawMain s = [ui]
  where
    editorFocus = case (s^.asMailIndex^.miMode) of
      BrowseMail -> False
      SearchMail -> True
    inputBox = E.renderEditor editorDrawContent editorFocus (s ^. asMailIndex ^. miSearchEditor)
    box = L.renderList listDrawElement False (s ^. asMailIndex ^. miListOfMails)
    ui = vBox [box, statusbar s, vLimit 1 inputBox]

statusbar :: AppState -> Widget Name
statusbar s =
    case s ^. asError of
        Just e -> withAttr "error" $ str e
        Nothing ->
            let l = s ^. asMailIndex ^. miListOfMails
                total = str $ show $ Vec.length $ l ^. (L.listElementsL)
            in withAttr "statusbar" $ str "Purebred: " <+>
               str "Item " <+> currentIndexW l <+> str " of " <+> total <+> fillLine

editorDrawContent :: [T.Text] -> Widget Name
editorDrawContent st = txt $ T.unlines st

fillLine :: Widget Name
fillLine = vLimit 1 (fill ' ')

listDrawElement :: Bool -> Mail -> Widget Name
listDrawElement sel a =
    let selStr w =
            if sel
                then withAttr L.listSelectedAttr w <+> fillLine
                else w
    in (selStr $
        padLeft (Pad 1) $
        hLimit 15 (str $ a ^. from) <+> padLeft (Pad 2) (str (a ^. subject)))

currentIndexW :: L.List Name Mail -> Widget Name
currentIndexW l = str $ show $ currentIndex l

currentIndex :: L.List Name Mail -> Int
currentIndex l = fromMaybe 0 $ l^.L.listSelectedL
