{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module UI.ComposeEditor.Main
  ( attachmentsEditor
  , renderPart
  , drawHeaders
  , renderConfirm
  ) where

import Brick.Types (Padding(..), Widget)
import Brick.Widgets.Core
       (hBox, padLeftRight, padLeft, padRight, padBottom, txt, withAttr, (<=>),
       (<+>), vLimit, hLimit, emptyWidget)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Brick.Widgets.Dialog (renderDialog)
import Brick.Widgets.Center (hCenter)
import Control.Lens (view, preview, to)
import qualified Data.Text as T
import Data.Text.Zipper (currentLine)

import Data.MIME
       (MIMEMessage, headers, contentType, CharsetLookup,
        filename, contentDisposition, isAttachment, showContentType)

import Config.Main (listSelectedAttr, listAttr)
import UI.Utils (takeFileName)
import UI.Views (focusedViewWidget)
import UI.Draw.Main (attachmentsHeader)
import Types

attachmentsEditor :: AppState -> Widget Name
attachmentsEditor s =
    let hasFocus = ComposeListOfAttachments == focusedViewWidget s
        attachmentsList =
          L.renderList (renderPart charsets) hasFocus (view (asCompose . cAttachments) s)
        charsets = view (asConfig . confCharsets) s
    in attachmentsHeader <=> attachmentsList

renderPart :: CharsetLookup -> Bool -> MIMEMessage -> Widget Name
renderPart charsets selected m =
  let pType = showContentType $ view (headers . contentType) m
      -- Only show the filename for now. See #253 for a discussion to fix this.
      pFilename =
        maybe "--" takeFileName (preview (headers . contentDisposition . filename charsets) m)
      listItemAttr = if selected then listSelectedAttr else listAttr
      attachmentType = txt (if isAttachment m then "A" else "I")
      widget = hBox
        [ padLeftRight 1 attachmentType
        , padRight Max (txt pFilename)
        , txt pType
        ]
  in withAttr listItemAttr widget

drawHeaders :: AppState -> Widget Name
drawHeaders s = padBottom (Pad 1) $ foldr (drawTableRows s) (txt T.empty) [ComposeSubject, ComposeTo, ComposeFrom]

-- | align labels to the right and values to the left, e.g.
--
--     Foo: bar
-- Subject: test
--
drawTableRows :: AppState -> Name -> Widget Name -> Widget Name
drawTableRows s name w = w
                         <=> vLimit 1
                         (hLimit 15 (padLeft Max (makeLabel name))
                          <+> padLeft (Pad 1) (txt (widgetValue name s)))

makeLabel :: Name -> Widget Name
makeLabel ComposeFrom = txt "From:"
makeLabel ComposeTo = txt "To:"
makeLabel _ = txt "Subject:"

widgetValue :: Name -> AppState -> T.Text
widgetValue ComposeFrom = view (asCompose . cFrom . E.editContentsL . to currentLine)
widgetValue ComposeTo = view (asCompose . cTo . E.editContentsL . to currentLine)
widgetValue ComposeSubject = view (asCompose . cSubject . E.editContentsL . to currentLine)
widgetValue _ = const T.empty

renderConfirm :: AppState -> Widget Name
renderConfirm s = renderDialog (view (asCompose . cKeepDraft) s) $ hCenter emptyWidget
