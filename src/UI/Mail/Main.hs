{-# LANGUAGE OverloadedStrings #-}
module UI.Mail.Main where

import qualified Brick.Main                as M
import           Brick.Types               (Padding (..), ViewportType (..),
                                            Widget)
import qualified Brick.Types               as T
import           Brick.Widgets.Core        (padLeft, padTop, txt, txtWrap,
                                            vLimit, viewport, (<+>), (<=>))
import qualified Brick.Widgets.List        as L
import           Codec.MIME.Type           (MIMEContent (..), MIMEParam (..),
                                            MIMEValue (..), Type (..),
                                            showMIMEType)
import           Control.Lens.Getter       ((^.))
import           Control.Lens.Setter       ((.~))
import           Control.Monad.IO.Class    (liftIO)
import qualified Data.Text                 as T
import           Graphics.Vty.Input.Events (Event)
import           UI.Index.Main             (renderMailList)
import           UI.Keybindings            (displayMailKeybindings, handleEvent,
                                            updateStateWithParsedMail)
import           UI.Status.Main            (statusbar)
import           UI.Types

-- | Instead of using the entire rendering area to show the email, we still show
-- the index in context above the mail.
--
-- Implementation detail: Currently we're creating the sub list of mails we show
-- for each key press. This might have to change in the future.
drawMail :: AppState -> [Widget Name]
drawMail s =
    [ (vLimit indexViewRows (renderMailList s)) <=>
      statusbar s <=>
      (viewport ScrollingMailView Vertical $
       (mailView (s ^. asMailView ^. mvMail)))]

-- | TODO: See #19
mailView :: Maybe ParsedMail -> Widget Name
mailView (Just (MIMEMail m)) =
    let filtered_headers =
            filter
                (\x ->
                      paramName x `elem` showHeaders) $
            mime_val_headers m
        widgets =
            (\h ->
                  txt (paramName h) <+> padLeft (Pad 1) (txtWrap (paramValue h))) <$>
            filtered_headers
        body = padTop (Pad 1) $ mimeContentToView m
    in foldr (<=>) (padTop (Pad 1) body) widgets
mailView (Just (RFC2822 _ _)) = txt "Not supported yet"
mailView Nothing = txt "Eeek: this is not supposed to happen"

mimeContentToView :: MIMEValue -> Widget Name
mimeContentToView (MIMEValue _ _ (Single m) _ _) = txtWrap m
mimeContentToView (MIMEValue _ _ (Multi xs) _ _) =
    let mval =
            filter
                (\x ->
                      showMIMEType (mimeType $ mime_val_type x) ==
                      preferContentType)
                xs
        picked =
            if length mval == 0
                then head xs
                else head mval
    in mimeContentToView picked

-- | The size limit of the index list
indexViewRows :: Int
indexViewRows = 10

preferContentType :: T.Text
preferContentType = "text/plain"

showHeaders :: [T.Text]
showHeaders = ["subject", "to", "from"]


-- | event handling for viewing a single mail

-- | The mail view shows a shortened list of mails. Forward all key strokes to
-- the list of mails by default.
mailEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
mailEvent s ev = handleEvent displayMailKeybindings displayMailDefault s ev

displayMailDefault :: AppState -> Event -> T.EventM Name (T.Next AppState)
displayMailDefault s ev = do
            l' <- L.handleListEvent ev (s ^. asMailIndex ^. miListOfMails)
            s' <- liftIO $ updateStateWithParsedMail (asMailIndex . miListOfMails .~ l' $ s)
            M.continue s'
