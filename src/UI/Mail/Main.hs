{-# LANGUAGE OverloadedStrings #-}

module UI.Mail.Main
  ( renderMailView
  , renderAttachmentsList
  ) where

import Brick.Types (Padding(..), ViewportType(..), Widget)
import qualified Brick.Widgets.List as L
import Brick.Widgets.Core
  (padTop, txt, txtWrap, viewport, (<+>), (<=>), withAttr, vBox,
   hBox, padLeftRight, padRight)

import Control.Lens (filtered, folded, toListOf, view, preview, has)
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)

import Data.MIME
import Storage.ParsedMail (chooseEntity, entityToText)

import Types
import UI.Draw.Main (attachmentsHeader)
import UI.Views (focusedViewWidget)
import Config.Main (headerKeyAttr, headerValueAttr, mailViewAttr,
                    listSelectedAttr, listAttr)

-- | Instead of using the entire rendering area to show the email, we still show
-- the index in context above the mail.
--
-- Implementation detail: Currently we're creating the sub list of mails we show
-- for each key press. This might have to change in the future.
renderMailView :: AppState -> Widget Name
renderMailView s = viewport ScrollingMailView Vertical (mailView s (view (asMailView . mvMail) s))

mailView :: AppState -> Maybe MIMEMessage -> Widget Name
mailView s (Just msg) = withAttr mailViewAttr $ messageToMailView s msg
mailView _ Nothing = txt "Eeek: this is not supposed to happen"

messageToMailView :: AppState -> MIMEMessage -> Widget Name
messageToMailView s msg =
  let
    wantHeader :: CI.CI B.ByteString -> Bool
    wantHeader = case view (asMailView . mvHeadersState) s of
      Filtered -> view (asConfig . confMailView . mvHeadersToShow) s
      ShowAll -> const True

    filteredHeaders =
      toListOf (headerList . folded . filtered (wantHeader . fst)) msg

    headerToWidget :: (CI.CI B.ByteString, B.ByteString) -> Widget Name
    headerToWidget (k, v) =
      withAttr headerKeyAttr $
        txt (decodeLenient (CI.original k) <> ": ")
        <+> withAttr headerValueAttr (txtWrap (decodeEncodedWords charsets v))

    headerWidgets = headerToWidget <$> filteredHeaders
    bodyWidget = padTop (Pad 1)
      (maybe (txt "No entity selected") (txtWrap . entityToText charsets) ent)
    preferredContentType = view (asConfig . confMailView . mvPreferredContentType) s
    ent = chooseEntity preferredContentType msg
    charsets = view (asConfig . confCharsets) s
  in
    vBox headerWidgets <=> padTop (Pad 1) bodyWidget

renderAttachmentsList :: AppState -> Widget Name
renderAttachmentsList s =
    let hasFocus = MailListOfAttachments == focusedViewWidget s
        attachmentsList =
          L.renderList (renderPart charsets) hasFocus (view (asMailView . mvAttachments) s)
        charsets = view (asConfig . confCharsets) s
    in attachmentsHeader <=> attachmentsList

-- TODO: Both these functions are basically duplicates. Use classes for
-- WireEntity and MIMEMessage to don't repeat our selfs?
-- See #264
renderPart :: CharsetLookup -> Bool -> WireEntity -> Widget Name
renderPart charsets selected m =
  let pType = showContentType $ view (headers . contentType) m
      pFilename = fromMaybe "--" (preview (headers . contentDisposition . filename charsets) m)
      listItemAttr = if selected then listSelectedAttr else listAttr
      attachmentType = txt (if isAttachment' m then "A" else "I")
      widget = hBox
        [ padLeftRight 1 attachmentType
        , padRight Max (txt pFilename)
        , txt pType
        ]
  in withAttr listItemAttr widget

isAttachment' :: WireEntity -> Bool
isAttachment' = has (headers . contentDisposition . dispositionType . filtered (== Attachment))
