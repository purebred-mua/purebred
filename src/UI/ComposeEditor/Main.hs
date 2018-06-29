{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module UI.ComposeEditor.Main (attachmentsEditor) where

import Brick.Types (Padding(..), Widget)
import Brick.Widgets.Core (padLeft, padRight, txt, (<+>), withAttr)
import qualified Brick.Widgets.List as L
import Network.Mail.Mime (Part(..))
import Control.Lens (view)
import Data.Maybe (fromMaybe)

import Config.Main (listSelectedAttr, listAttr)
import UI.Utils (focusedViewWidget)
import Types

attachmentsEditor :: AppState -> Widget Name
attachmentsEditor s =
    let hasFocus = ListOfAttachments == focusedViewWidget s ComposeFrom
        attachmentsList = L.renderList renderPart hasFocus (view (asCompose . cAttachments) s)
    in attachmentsList

renderPart :: Bool -> MailPart -> Widget Name
renderPart selected (MailPart aType p) =
  let pType = partType p
      pFilename = fromMaybe "--" (partFilename p)
      listItemAttr = if selected then listSelectedAttr else listAttr
      attachmentType Inline = txt "I"
      attachmentType _ = txt "A"
      widget = padRight (Pad 1) (padLeft (Pad 1) $ attachmentType aType) <+> padRight Max (txt pFilename) <+> txt pType
  in withAttr listItemAttr widget
