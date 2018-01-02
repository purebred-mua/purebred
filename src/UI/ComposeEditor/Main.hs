{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module UI.ComposeEditor.Main where

import Brick.Types (Padding(..), Widget)
import Brick.Widgets.Core
       (fill, hLimit, padRight, padTop, str, txt, vLimit, withAttr, (<+>),
        (<=>))
import qualified Brick.Widgets.Edit           as E
import qualified Brick.Widgets.List           as L
import Control.Lens.Fold ((^?))
import Control.Lens.Getter (view)
import Control.Lens.Lens (Lens')
import qualified Data.Text                    as T
import Data.Vector (fromList)

import UI.Draw.Main (editorDrawContent)
import Types

drawComposeEditor :: AppState -> [Widget Name]
drawComposeEditor s = [ui <=> attachmentsEditor s]
  where
    ui = foldr (drawTableRows s) (txt T.empty) [GatherHeadersSubject, GatherHeadersTo, GatherHeadersFrom]

-- | align labels to the right and values to the left, e.g.
--
--     Foo: bar
-- Subject: test
--
drawTableRows :: AppState -> Mode -> Widget Name -> Widget Name
drawTableRows s m w =
    w
    <=> vLimit 1
    (hLimit 15 (padRight Max (getLabelForComposeState m))
     <+> E.renderEditor editorDrawContent False (view (asCompose . focusedLens m) s))

attachmentsEditor :: AppState -> Widget Name
attachmentsEditor s =
    let attachmentsStatus =
            withAttr "statusbar" $
            txt "-- Attachments " <+> vLimit 1 (fill '-')
        aList = L.list ListOfMails (fromList [view asCompose s ^? cTmpFile]) 1
        attachmentsList =
            L.renderList
                (\_ i ->
                      str $ show i)
                False
                aList
    in padTop (Pad 1) attachmentsStatus <=> attachmentsList

focusedLens :: Mode -> Lens' Compose (E.Editor T.Text Name)
focusedLens GatherHeadersFrom = cFrom
focusedLens GatherHeadersTo = cTo
focusedLens _ = cSubject

getLabelForComposeState :: Mode -> Widget Name
getLabelForComposeState GatherHeadersFrom = txt "From:"
getLabelForComposeState GatherHeadersTo = txt "To:"
getLabelForComposeState _ = txt "Subject:"
