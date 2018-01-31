{-# LANGUAGE OverloadedStrings #-}

module UI.Mail.Main where

import Brick.Types (Padding(..), ViewportType(..), Widget)
import Brick.Widgets.Core
       (padLeft, padTop, txt, txtWrap, vLimit, viewport, (<+>), (<=>),
        withAttr)

import Control.Applicative ((<|>))
import Control.Lens (filtered, firstOf, folded, preview, toListOf, view)
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))

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

mailView :: AppState -> Maybe (Message MIME) -> Widget Name
mailView s (Just msg) = messageToMailView s msg
mailView _ Nothing = txt "Eeek: this is not supposed to happen"

messageToMailView :: AppState -> Message MIME -> Widget Name
messageToMailView s msg =
  let
    wantHeader :: CI.CI B.ByteString -> Bool
    wantHeader = case view (asMailView . mvHeadersState) s of
      Filtered -> view (asConfig . confMailView . mvHeadersToShow) s
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
chooseEntity s msg =
  let
    preferredContentType = view (asConfig . confMailView . mvPreferredContentType) s
    match :: Entity -> Bool
    match (h, _) = view contentType h `ctEq` preferredContentType

    -- select first entity with matching content-type;
    -- otherwise select first entity;
    ent = firstOf (entities . filtered match) msg <|> firstOf entities msg
  in
    fromMaybe mempty ent

entityToView :: Entity -> Widget Name
entityToView ent@(h, b) =
  txtWrap $ decodeLenient $ fromMaybe
    ("NOTE: transfer decoding failed (" <> cte <> "). Showing raw body.\n\n" <> b)
      -- Note: when we have an AST for content display, we can make
      -- the above an "alert" instead of prepending to actual message
    (preview contentTransferDecoded ent)
  where
    cte = fromMaybe "" (preview (header "content-transfer-encoding") h)


-- | The size limit of the index list
indexViewRows :: AppState -> Int
indexViewRows = view (asConfig . confMailView . mvIndexRows)
