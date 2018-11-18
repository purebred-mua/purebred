{-# LANGUAGE OverloadedStrings #-}

module UI.Mail.Main (renderMailView) where

import Brick.Types (Padding(..), ViewportType(..), Widget)
import Brick.Widgets.Core
  (padTop, txt, txtWrap, viewport, (<+>), (<=>), withAttr, vBox)

import Control.Lens (filtered, folded, toListOf, view)
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import Data.Semigroup ((<>))

import Data.MIME
import Storage.ParsedMail (chooseEntity, entityToText)

import Types
import Config.Main (headerKeyAttr, headerValueAttr, mailViewAttr)

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
        <+> withAttr headerValueAttr (txtWrap (decodeEncodedWords v))

    headerWidgets = headerToWidget <$> filteredHeaders
    bodyWidget = padTop (Pad 1) (maybe (txt "No entity selected") entityToView ent)
    preferredContentType = view (asConfig . confMailView . mvPreferredContentType) s
    ent = chooseEntity preferredContentType msg
  in
    vBox headerWidgets <=> padTop (Pad 1) bodyWidget

entityToView :: WireEntity -> Widget Name
entityToView = txtWrap . entityToText
