{-# LANGUAGE OverloadedStrings #-}

module UI.Mail.Main (renderMailView) where

import Brick.Types (Padding(..), ViewportType(..), Widget)
import Brick.Widgets.Core
  (padTop, txt, txtWrap, viewport, (<+>), (<=>), withAttr)

import Control.Applicative ((<|>))
import Control.Lens (filtered, firstOf, folded, to, toListOf, view)
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import Data.Semigroup ((<>))
import Data.Text.Lens (packed)
import qualified Data.Text as T

import Data.MIME

import Types
import Config.Main (headerKeyAttr, headerValueAttr)

-- | Instead of using the entire rendering area to show the email, we still show
-- the index in context above the mail.
--
-- Implementation detail: Currently we're creating the sub list of mails we show
-- for each key press. This might have to change in the future.
renderMailView :: AppState -> Widget Name
renderMailView s = viewport ScrollingMailView Vertical (mailView s (view (asMailView . mvMail) s))

mailView :: AppState -> Maybe MIMEMessage -> Widget Name
mailView s (Just msg) = messageToMailView s msg
mailView _ Nothing = txt "Eeek: this is not supposed to happen"

messageToMailView :: AppState -> MIMEMessage -> Widget Name
messageToMailView s msg =
  let
    wantHeader :: CI.CI B.ByteString -> Bool
    wantHeader = case view (asMailView . mvHeadersState) s of
      Filtered -> view (asConfig . confMailView . mvHeadersToShow) s
      ShowAll -> const True

    filteredHeaders =
      toListOf (headers . folded . filtered (wantHeader . fst)) msg

    headerToWidget :: (CI.CI B.ByteString, B.ByteString) -> Widget Name
    headerToWidget (k, v) =
      withAttr headerKeyAttr $
        txt (decodeLenient (CI.original k) <> ": ")
        <+> withAttr headerValueAttr (txtWrap (decodeEncodedWords v))

    headerWidgets = headerToWidget <$> filteredHeaders
    bodyWidget = padTop (Pad 1) (maybe (txt "No entity selected") entityToView ent)
    ent = chooseEntity s msg
  in
    foldr (<=>) (padTop (Pad 1) bodyWidget) headerWidgets

chooseEntity :: AppState -> MIMEMessage -> Maybe WireEntity
chooseEntity s msg =
  let
    preferredContentType = view (asConfig . confMailView . mvPreferredContentType) s
    match = ctEq preferredContentType . view (headers . contentType)

    -- select first entity with matching content-type;
    -- otherwise select first entity;
  in firstOf (entities . filtered match) msg <|> firstOf entities msg

entityToView :: WireEntity -> Widget Name
entityToView msg = txtWrap . either err (view body) $
  view transferDecoded msg >>= view charsetDecoded
  where
    err :: EncodingError -> T.Text
    err e =
      "ERROR: " <> view (to show . packed) e <> ". Showing raw body.\n\n"
      <> decodeLenient (view body msg)
