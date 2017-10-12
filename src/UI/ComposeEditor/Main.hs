{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module UI.ComposeEditor.Main where

import Brick.Main (continue)
import Brick.Types (EventM, Next, Padding(..), Widget)
import Brick.Widgets.Core
       (fill, hLimit, padRight, padTop, str, txt, vLimit, withAttr, (<+>),
        (<=>))
import qualified Brick.Widgets.Edit           as E
import qualified Brick.Widgets.List           as L
import Control.Lens.Fold ((^?))
import Control.Lens.Getter (view)
import Control.Lens.Lens (Lens')
import qualified Data.Text                    as T
import Data.Vector (fromList)
import Graphics.Vty (Event)

import UI.Draw.Main (editorDrawContent)
import UI.Keybindings (handleEvent)
import Types

drawComposeEditor :: AppState -> [Widget Name]
drawComposeEditor s = [ui <=> attachmentsEditor s]
  where
    ui = foldr (drawTableRows s) (txt T.empty) [AskSubject, AskFrom, AskTo]

-- | align labels to the right and values to the left, e.g.
--
--     Foo: bar
-- Subject: test
--
drawTableRows :: AppState -> ComposeState -> Widget Name -> Widget Name
drawTableRows s cs w =
    w <=>
    hLimit 15 (padRight Max (getLabelForComposeState cs)) <+>
     E.renderEditor editorDrawContent False (view (asCompose . focusedLens cs) s)

attachmentsEditor :: AppState -> Widget Name
attachmentsEditor s =
    let attachmentsStatus =
            withAttr "statusbar" $
            txt "-- Attachments " <+> vLimit 1 (fill '-')
        aList = L.list ListOfMails (fromList [view asCompose s ^? cTmpFile]) 1
        attachmentsList =
            L.renderList
                (\_ i ->
                      str $ show i)
                False
                aList
    in padTop (Pad 1) attachmentsStatus <=> attachmentsList

focusedLens :: ComposeState -> Lens' Compose (E.Editor T.Text Name)
focusedLens AskFrom = cFrom
focusedLens AskTo = cTo
focusedLens AskSubject = cSubject

getLabelForComposeState :: ComposeState -> Widget Name
getLabelForComposeState AskFrom = txt "From:"
getLabelForComposeState AskTo = txt "To:"
getLabelForComposeState AskSubject = txt "Subject:"

-- | event handling for composing e-mail

-- | the editor which shows header fields and attachments
composeEditor :: AppState
              -> Event
              -> EventM Name (Next AppState)
composeEditor s =
    handleEvent
        (view (asConfig . confComposeView . cvKeybindings) s)
        (\s' _ ->
              continue s')
        s
