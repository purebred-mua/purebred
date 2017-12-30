{-# LANGUAGE OverloadedStrings #-}

module UI.Mail.Main where

import Brick.Types (Padding(..), ViewportType(..), Widget)
import Brick.Widgets.Core
       (padLeft, padTop, txt, txtWrap, vLimit, viewport, (<+>), (<=>),
        withAttr)

import Control.Applicative ((<|>))
import Control.Lens (filtered, firstOf, folded, toListOf, view)
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import Data.Maybe (fromMaybe)
import Data.Profunctor (lmap)
import qualified Data.Text as T

import Codec.MIME.Type
       (MIMEContent(..), MIMEParam(..), MIMEValue(..), Type(..),
        showMIMEType)

import Data.MIME

import UI.Index.Main (renderMailList, renderEditor)
import UI.Status.Main (statusbar)
import Types
import Config.Main (headerKeyAttr, headerValueAttr)

-- | Instead of using the entire rendering area to show the email, we still show
-- the index in context above the mail.
--
-- Implementation detail: Currently we're creating the sub list of mails we show
-- for each key press. This might have to change in the future.
drawMail :: AppState -> [Widget Name]
drawMail s =
    [ vLimit (indexViewRows s) (renderMailList s) <=>
      statusbar s <=>
      viewport ScrollingMailView Vertical (mailView s (view (asMailView . mvMail) s)) <=>
      vLimit 1 (renderEditor s)
    ]

-- | TODO: See #19
mailView :: AppState -> Maybe ParsedMail -> Widget Name
mailView s (Just (MIMEMail m)) =
    let filtered_headers = filter (headerFilter s . CI.mk . paramName) $ mime_val_headers m
        widgets =
            (\h ->
                  withAttr headerKeyAttr $
                  txt (paramName h) <+>
                  padLeft
                      (Pad 1)
                      (withAttr headerValueAttr $ txtWrap (paramValue h))) <$>
            filtered_headers
        body = padTop (Pad 1) $ mimeContentToView s m
    in foldr (<=>) (padTop (Pad 1) body) widgets
mailView s (Just (PurebredEmail msg)) = messageToMailView s msg
mailView _ Nothing = txt "Eeek: this is not supposed to happen"

messageToMailView :: AppState -> Message MIME -> Widget Name
messageToMailView s msg =
  let
    wantHeader :: CI.CI B.ByteString -> Bool
    wantHeader = case view (asMailView . mvHeadersState) s of
      Filtered ->
        -- FIXME the config is currently (CI T.Text -> Bool)
        lmap (CI.map decodeLenient) $
        view (asConfig . confMailView . mvHeadersToShow) s
      ShowAll -> const True

    filteredHeaders =
      toListOf (messageHeaders . folded . filtered (wantHeader . fst)) msg

    headerToWidget :: (CI.CI B.ByteString, B.ByteString) -> Widget Name
    headerToWidget (k, v) =
      withAttr headerKeyAttr $
      txt (decodeLenient (CI.original k)) <+>
      padLeft
          (Pad 1)
          (withAttr headerValueAttr (txtWrap (decodeLenient v)))

    headerWidgets = headerToWidget <$> filteredHeaders
    body = padTop (Pad 1) (entityToView ent)
    ent = chooseEntity s msg
  in
    foldr (<=>) (padTop (Pad 1) body) headerWidgets

chooseEntity :: AppState -> Message MIME -> Entity
chooseEntity _ msg =
  let
    preferredContentType = defaultContentType -- FIXME
    match :: Entity -> Bool
    match (h, _) = view contentType h `ctEq` preferredContentType

    -- select first entity with matching content-type;
    -- otherwise select first entity;
    ent = firstOf (entities . filtered match) msg <|> firstOf entities msg
  in
    fromMaybe mempty ent

entityToView :: Entity -> Widget Name
entityToView (_, b) = txtWrap $ decodeLenient b

mimeContentToView :: AppState -> MIMEValue -> Widget Name
mimeContentToView _ (MIMEValue _ _ (Single m) _ _) = txtWrap m
mimeContentToView s (MIMEValue _ _ (Multi xs) _ _) =
    let mval =
            filter
                (\x ->
                      showMIMEType (mimeType $ mime_val_type x) ==
                      preferContentType s)
                xs
        picked =
            if null mval
                then head xs  -- FIXME non-total
                else head mval
    in mimeContentToView s picked

-- | The size limit of the index list
indexViewRows :: AppState -> Int
indexViewRows = view (asConfig . confMailView . mvIndexRows)

preferContentType :: AppState -> T.Text
preferContentType = view (asConfig . confMailView . mvPreferredContentType)

headerFilter :: AppState -> (CI.CI T.Text -> Bool)
headerFilter s =
    case view (asMailView . mvHeadersState) s of
        Filtered -> view (asConfig . confMailView . mvHeadersToShow) s
        ShowAll -> const True
