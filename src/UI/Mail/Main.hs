{-# LANGUAGE OverloadedStrings #-}

module UI.Mail.Main where

import qualified Brick.Main as M
import Brick.Types (Padding(..), ViewportType(..), Widget)
import qualified Brick.Types as T
import Brick.Widgets.Core
       (padLeft, padTop, txt, txtWrap, vLimit, viewport, (<+>), (<=>),
        withAttr)
import qualified Brick.Widgets.List as L

import Codec.MIME.Type
       (MIMEContent(..), MIMEParam(..), MIMEValue(..), Type(..),
        showMIMEType)
import qualified Data.CaseInsensitive as CI
import Control.Lens.Getter (view)
import Control.Lens.Setter (set)
import Data.CaseInsensitive (mk)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Graphics.Vty.Input.Events (Event)
import UI.Index.Keybindings
       (updateStateWithParsedMail, updateReadState)
import Storage.Notmuch (removeTag)
import UI.Index.Main (renderMailList)
import UI.Keybindings (handleEvent)
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
      viewport ScrollingMailView Vertical (mailView s (view (asMailView . mvMail) s))
    ]

-- | TODO: See #19
mailView :: AppState -> Maybe ParsedMail -> Widget Name
mailView s (Just (MIMEMail m)) =
    let filtered_headers = filter (headerFilter s . mk . paramName) $ mime_val_headers m
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
mailView _ (Just (RFC2822 _ _)) = txt "Not supported yet"
mailView _ Nothing = txt "Eeek: this is not supposed to happen"

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

-- | event handling for viewing a single mail

-- | The mail view shows a shortened list of mails. Forward all key strokes to
-- the list of mails by default.
mailEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
mailEvent s =
    handleEvent
        (view (asConfig . confMailView . mvKeybindings) s)
        displayMailDefault
        s

displayMailDefault :: AppState -> Event -> T.EventM Name (T.Next AppState)
displayMailDefault s ev = do
            l' <- L.handleListEvent ev (view (asMailIndex . miListOfMails) s)
            s' <- liftIO $ updateStateWithParsedMail (set (asMailIndex . miListOfMails) l' s)
                  >>= updateReadState removeTag
            M.continue s'
