{-# LANGUAGE OverloadedStrings #-}

module UI.Index.Main where

import Brick.Types (Padding(..), Widget)
import Brick.AttrMap (AttrName)
import Brick.Widgets.Core
       (hLimit, padLeft, txt, vBox, vLimit, withAttr, (<+>))
import qualified Brick.Widgets.List as L
import Control.Lens.Getter (view)
import qualified Data.ByteString as B
import Data.Time.Clock (UTCTime(..))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Semigroup ((<>))
import Data.Text as T (Text, pack, unwords)

import Notmuch (getTag)

import UI.Draw.Main (renderEditorWithLabel, fillLine)
import UI.Status.Main (statusbar)
import Storage.Notmuch (hasTag)
import Types
import Config.Main
       (listNewMailAttr, listNewMailSelectedAttr, mailTagsAttr,
        listSelectedAttr, listAttr, mailAuthorsAttr)

drawMain :: AppState -> [Widget Name]
drawMain s = [ui]
  where
    inputBox = renderEditor s
    ui = vBox [renderMailList s, statusbar s, vLimit 1 inputBox]

renderEditor :: AppState -> Widget Name
renderEditor s = let editorFocus = view asAppMode s `elem` [SearchThreads, ManageThreadTags, ManageMailTags]
                     render = renderEditorWithLabel s editorFocus
                 in case view asAppMode s of
                      ManageThreadTags -> render (view (asMailIndex . miThreadTagsEditor) s)
                      ManageMailTags -> render (view (asMailIndex . miMailTagsEditor) s)
                      _ -> render (view (asMailIndex . miSearchThreadsEditor) s)

renderMailList :: AppState -> Widget Name
renderMailList s =
    let listOfThreads = L.renderList (listDrawThread s) True $ view (asMailIndex . miListOfThreads) s
        listOfMails = L.renderList (listDrawMail s) True $ view (asMailIndex . miListOfMails) s
    in if view asAppMode s `elem`
          [ManageThreadTags, BrowseThreads, SearchThreads]
           then listOfThreads
           else listOfMails

listDrawMail :: AppState -> Bool -> NotmuchMail -> Widget Name
listDrawMail s sel a =
    let settings = view (asConfig . confNotmuch) s
        isNewMail = hasTag (view nmNewTag settings) a
        widget = padLeft (Pad 1) (txt $ formatDate (view mailDate a)) <+>
                 padLeft (Pad 1) (renderTagsWidget (view mailTags a) (view nmNewTag settings)) <+>
                 padLeft (Pad 1) (renderAuthors $ view mailFrom a) <+>
                 padLeft (Pad 1) (txt (view mailSubject a)) <+> fillLine
    in withAttr (getListAttr isNewMail sel) widget

listDrawThread :: AppState -> Bool -> NotmuchThread -> Widget Name
listDrawThread s sel a =
    let settings = view (asConfig . confNotmuch) s
        isNewMail = hasTag (view nmNewTag settings) a
        widget = padLeft (Pad 1) (txt $ formatDate (view thDate a)) <+>
                 padLeft (Pad 1) (txt $ pack $ "(" <> show (view thReplies a) <> ")") <+>
                 padLeft (Pad 1) (renderTagsWidget (view thTags a) (view nmNewTag settings)) <+>
                 padLeft (Pad 1) (renderAuthors $ T.unwords $ view thAuthors a) <+>
                 padLeft (Pad 1) (txt (view thSubject a)) <+> fillLine
    in withAttr (getListAttr isNewMail sel) widget

getListAttr :: Bool  -- ^ new?
            -> Bool  -- ^ selected?
            -> AttrName
getListAttr True True = listNewMailSelectedAttr  -- new and selected
getListAttr True False = listNewMailAttr  -- new and not selected
getListAttr False True = listSelectedAttr  -- not new but selected
getListAttr False False = listAttr  -- not selected and not new

formatDate :: UTCTime -> Text
formatDate t = pack $ formatTime defaultTimeLocale "%d/%b" (utctDay t)

renderAuthors :: Text -> Widget Name
renderAuthors authors = withAttr mailAuthorsAttr $ hLimit 15 (txt authors)

renderTagsWidget :: [Tag] -> Tag -> Widget Name
renderTagsWidget tgs ignored =
    let ts = filter (/= ignored) tgs
    in withAttr mailTagsAttr $ vLimit 1 $ txt $ decodeLenient $ B.intercalate " " $ fmap getTag ts
