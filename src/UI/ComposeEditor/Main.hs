{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module UI.ComposeEditor.Main where

import Brick.Types (Padding(..), Widget)
import qualified Brick.Focus as Brick
import Brick.Widgets.Core
       (fill, hLimit, padRight, padTop, txt, vLimit, withAttr, (<+>),
        (<=>))
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Network.Mail.Mime (Part(..))
import Control.Lens (Lens', view)
import Data.Maybe (fromMaybe)
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
     <+> Brick.withFocusRing focusring (E.renderEditor editorDrawContent) focusedInput)

attachmentsEditor :: AppState -> Widget Name
attachmentsEditor s =
    let attachmentsStatus = withAttr "statusbar" $ txt "-- Attachments " <+> vLimit 1 (fill '-')
        hasFocus = Just ListOfAttachments == Brick.focusGetCurrent (view (asCompose . cFocusFields) s)
        attachmentsList = L.renderList (\_ i -> renderPart i) hasFocus (view (asCompose . cAttachments) s)
    in padTop (Pad 1) attachmentsStatus <=> attachmentsList

renderPart :: Part -> Widget Name
renderPart p = let pType = partType p
                   pFilename = fromMaybe "--" (partFilename p)
               in padRight Max (txt pFilename) <+> txt pType

getLabelForComposeState :: Name -> Widget Name
getLabelForComposeState ComposeFrom = txt "From:"
getLabelForComposeState ComposeTo = txt "To:"
getLabelForComposeState _ = txt "Subject:"

focusedLens :: Name -> Lens' Compose (E.Editor T.Text Name)
focusedLens ComposeFrom = cFrom
focusedLens ComposeTo = cTo
focusedLens _ = cSubject
