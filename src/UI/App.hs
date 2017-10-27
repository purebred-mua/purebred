{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
-- | The main application module
module UI.App where

import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Graphics.Vty.Input.Events (Event)
import Control.Lens.Getter (view)
import Control.Monad.Except (runExceptT)
import System.Exit (die)

import Storage.Notmuch (getMessages)
import UI.Keybindings (handleEvent)
import UI.ComposeEditor.Main (composeEditor, drawComposeEditor)
import UI.GatherHeaders.Main
       (drawInteractiveHeaders, interactiveGatherHeaders)
import UI.Index.Main (drawMain, mainEvent, listEventDefault)
import UI.Actions (initialCompose)
import UI.Mail.Main (drawMail, mailEvent)
import UI.Help.Main (drawHelp, handleHelpEvents)
import Data.Proxy
import Types

drawUI :: AppState -> [Widget Name]
drawUI s =
    case view asAppMode s of
        BrowseMail -> drawMain s
        SearchMail -> drawMain s
        ManageTags -> drawMain s
        ViewMail -> drawMail s
        GatherHeaders -> drawInteractiveHeaders s
        ComposeEditor -> drawComposeEditor s
        Help -> drawHelp s

class EventHandler (m :: Mode) where
  handle :: Proxy m -> AppState -> Event -> T.EventM Name (T.Next AppState)
  fallback :: Proxy m -> AppState -> Event -> T.EventM Name (T.Next AppState)

instance EventHandler 'BrowseMail where
  handle _ s e = handleEvent (view (asConfig . confIndexView . ivKeybindings) s) listEventDefault s e

instance EventHandler 'SearchMail where
  handle _ s e = handleEventLensed

instance EventHandler 'ManageTags where
  handle _ s e = mainEvent s e

dispatch :: EventHandler m => Proxy m -> AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
dispatch m s (T.VtyEvent e) = handle m s e

appEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
appEvent s (T.VtyEvent ev) =
  case view asAppMode s of
    BrowseMail -> handle (Proxy :: Proxy 'BrowseMail) s ev
    SearchMail -> handle (Proxy :: Proxy 'SearchMail) s ev
    ManageTags -> handle (Proxy :: Proxy 'ManageTags) s ev
    ViewMail -> handle (Proxy :: Proxy 'ViewMail) s ev
    GatherHeaders -> handle (Proxy :: Proxy 'GatherHeaders) s ev
    ComposeEditor -> handle (Proxy :: Proxy 'ComposeEditor) s ev
    Help -> handle (Proxy :: Proxy 'Help) s ev
{-
case ev of
  T.VtyEvent e ->
    case view asAppMode s of
        BrowseMail -> mainEvent s e
        SearchMail -> mainEvent s e
        ManageTags -> mainEvent s e
        ViewMail -> mailEvent s e
        GatherHeaders -> interactiveGatherHeaders s e
        ComposeEditor -> composeEditor s e
        Help -> handleHelpEvents s e
  _ -> M.continue s  -- we only handle Vty events
-}

initialState :: InternalConfiguration -> IO AppState
initialState conf = do
    let searchterms = view (confNotmuch . nmSearch) conf
    r <- runExceptT $ getMessages searchterms (view confNotmuch conf)
    case r of
      Left e -> die $ show e  -- TODO don't crash?
      Right vec ->
        let
          mi = MailIndex
                (L.list ListOfMails vec 1)
                (E.editor
                     EditorInput
                     Nothing
                     searchterms)
          mv = MailView Nothing Filtered
        in pure $ AppState conf mi mv initialCompose BrowseMail Nothing

theApp :: AppState -> M.App AppState e Name
theApp s =
    M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return
    , M.appAttrMap = const (view (asConfig . confColorMap) s)
    }
