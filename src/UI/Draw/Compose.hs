{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module UI.Draw.Compose where

import           Brick.Types         (Padding (..), Widget)
import           Brick.Widgets.Core  (fill, padRight, padTop, str, txt, vBox,
                                      vLimit, hLimit, withAttr, (<+>), (<=>))
import qualified Brick.Widgets.Edit  as E
import qualified Brick.Widgets.List  as L
import           Control.Lens.Fold   ((^?))
import           Control.Lens.Getter ((^.))
import           Control.Lens.Lens   (Lens')
import qualified Data.Text           as T
import           Data.Vector         (fromList)
import           UI.Draw.Main        (listDrawElement, statusbar)
import           UI.Types

drawInteractiveHeaders :: AppState -> [Widget Name]
drawInteractiveHeaders s = [ui]
  where
    inputBox = E.renderEditor editorDrawContent True (focusedEditor s)
    inputPrefix = padRight (Pad 1) $ getLabelForComposeState (s ^. asCompose ^. cFocus)
    box =
        L.renderList listDrawElement False (s ^. asMailIndex ^. miListOfMails)
    ui = vBox [box, statusbar s, inputPrefix <+> vLimit 1 inputBox]

drawComposeEditor :: AppState -> [Widget Name]
drawComposeEditor s = [ui <=> attachmentsEditor s]
  where
    ui =
        foldr
            (drawTableRows s)
            (txt "")
            [AskSubject, AskFrom, AskTo]

-- | align labels to the right and values to the left, e.g.
--
--     Foo: bar
-- Subject: test
--
drawTableRows :: AppState -> ComposeState -> Widget Name -> Widget Name
drawTableRows s cs w =
    w <=>
    (hLimit 15 $ padRight Max (getLabelForComposeState cs) <+>
     editorDrawContent
         (E.getEditContents $ (s ^. asCompose . (focusedLens cs))))

attachmentsEditor :: AppState -> Widget Name
attachmentsEditor s =
    let attachmentsStatus =
            withAttr "statusbar" $
            txt "-- Attachments " <+> vLimit 1 (fill '-')
        aList = L.list ListOfMails (fromList [s ^. asCompose ^? cTmpFile]) 1
        attachmentsList =
            L.renderList
                (\_ i ->
                      str $ show i)
                False
                aList
    in (padTop (Pad 1) $ attachmentsStatus) <=> attachmentsList

editorDrawContent :: [T.Text] -> Widget Name
editorDrawContent st = txt $ T.unlines st

focusedEditor :: AppState -> E.Editor T.Text Name
focusedEditor s =
    case s ^. asCompose ^. cFocus of
        AskFrom -> s ^. asCompose ^. cFrom
        AskTo -> s ^. asCompose ^. cTo
        AskSubject -> s ^. asCompose ^. cSubject

focusedLens :: ComposeState -> Lens' Compose (E.Editor T.Text Name)
focusedLens AskFrom = cFrom
focusedLens AskTo = cTo
focusedLens AskSubject = cSubject

getLabelForComposeState :: ComposeState -> Widget Name
getLabelForComposeState AskFrom = txt "From:"
getLabelForComposeState AskTo = txt "To:"
getLabelForComposeState AskSubject = txt "Subject:"
