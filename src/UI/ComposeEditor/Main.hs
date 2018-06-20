{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module UI.ComposeEditor.Main (attachmentsEditor) where

import Brick.Types (Padding(..), Widget)
import Brick.Widgets.Core (padRight, txt, (<+>), withAttr)
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

renderPart :: Bool -> Part -> Widget Name
renderPart selected p =
  let pType = partType p
      pFilename = fromMaybe "--" (partFilename p)
      listItemAttr = if selected then listSelectedAttr else listAttr
      widget = padRight Max (txt pFilename) <+> txt pType
  in withAttr listItemAttr widget
