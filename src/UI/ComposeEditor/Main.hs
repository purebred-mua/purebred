{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module UI.ComposeEditor.Main (attachmentsEditor) where

import Brick.Types (Padding(Max), Widget)
import Brick.Widgets.Core (hBox, padLeftRight, padRight, txt, withAttr)
import qualified Brick.Widgets.List as L
import Control.Lens (view, preview)
import Data.Maybe (fromMaybe)

import Data.MIME
       (MIMEMessage, headers, contentType,
        filename, contentDisposition, isAttachment, showContentType)

import Config.Main (listSelectedAttr, listAttr)
import UI.Utils (focusedViewWidget)
import Types

attachmentsEditor :: AppState -> Widget Name
attachmentsEditor s =
    let hasFocus = ListOfAttachments == focusedViewWidget s ComposeFrom
        attachmentsList = L.renderList renderPart hasFocus (view (asCompose . cAttachments) s)
    in attachmentsList

renderPart :: Bool -> MIMEMessage -> Widget Name
renderPart selected m =
  let pType = showContentType $ view (headers . contentType) m
      pFilename = fromMaybe "--" (preview (headers . contentDisposition . filename) m)
      listItemAttr = if selected then listSelectedAttr else listAttr
      attachmentType = txt (if isAttachment m then "A" else "I")
      widget = hBox
        [ padLeftRight 1 attachmentType
        , padRight Max (txt pFilename)
        , txt pType
        ]
  in withAttr listItemAttr widget
