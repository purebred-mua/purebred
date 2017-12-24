{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module UI.ComposeEditor.Main where

import Brick.Types (Padding(..), Widget)
import qualified Brick.Focus as Brick
import Brick.Widgets.Core
       (fill, hLimit, padRight, padTop, str, txt, vLimit, withAttr, (<+>),
        (<=>))
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Control.Lens (Lens', view, traversed)
import Data.Vector.Lens (toVectorOf)
import qualified Data.Text as T

import UI.Draw.Main (editorDrawContent)
import Types

drawComposeEditor :: AppState -> [Widget Name]
drawComposeEditor s = [ui <=> attachmentsEditor s]
  where
    ui = foldr (drawTableRows s) (txt T.empty) [ComposeSubject, ComposeTo, ComposeFrom]

-- | align labels to the right and values to the left, e.g.
--
--     Foo: bar
-- Subject: test
--
drawTableRows :: AppState -> Name -> Widget Name -> Widget Name
drawTableRows s name w =
  let focusring = view (asCompose . cFocusFields) s
      focusedInput = view (cFocusedEditorL name) s
  in w
    <=> vLimit 1
    (hLimit 15 (padRight Max (getLabelForComposeState name))
     <+> Brick.withFocusRing focusring (E.renderEditor  editorDrawContent) focusedInput)

attachmentsEditor :: AppState -> Widget Name
attachmentsEditor s =
    let attachmentsStatus =
            withAttr "statusbar" $
            txt "-- Attachments " <+> vLimit 1 (fill '-')
        aList = L.list ListOfMails (toVectorOf (asCompose . cTmpFile . traversed) s) 1
        attachmentsList =
            L.renderList
                (\_ i ->
                      str $ show i)
                False
                aList
    in padTop (Pad 1) attachmentsStatus <=> attachmentsList

getLabelForComposeState :: Name -> Widget Name
getLabelForComposeState ComposeFrom = txt "From:"
getLabelForComposeState ComposeTo = txt "To:"
getLabelForComposeState _ = txt "Subject:"

focusedLens :: Mode -> Lens' Compose (E.Editor T.Text Name)
focusedLens GatherHeadersFrom = cFrom
focusedLens GatherHeadersTo = cTo
focusedLens _ = cSubject
