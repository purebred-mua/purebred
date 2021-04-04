-- copied from https://github.com/rootmos/haskeline/blob/master/System/Console/Haskeline/Brick.hs
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Brick.Haskeline
  ( configure,
    initialWidget,
    Widget,
    Config,
    ToBrick,
    handleEditorEvent,
    handleAppEvent,
    render,
    visibleLinesL,
    fromBrickChan,
    useBrick
  )
where

import Brick hiding (Widget, render)
import qualified Brick as B
import qualified Brick.BChan as BC
import GHC.Conc (atomically)
import Control.Concurrent
import Control.Concurrent.STM (TChan, readTChan, newTChan, writeTChan)
import Control.Lens (Lens, Lens', lens)
import qualified Data.Text as T
import qualified Data.Text.Zipper as Z hiding (textZipper)
import qualified Graphics.Vty as V
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Catch (MonadMask, MonadCatch, MonadThrow)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT))
import System.Console.Haskeline.Internal (
  Event(..), Key (..), Term(..), TermOps(..), hasShift,
  simpleKey, Layout(..), LineChars, BaseKey(..), RunTerm(..),
  CommandMonad, EvalTerm(..), setControlBits, ctrlKey,
  metaKey, saveKeys, graphemesToString, MonadReader(..), Behavior(..))

data Config e = MkConfig
  { _fromBrickChan :: TChan Event,
    toAppChan :: BC.BChan e,
    toAppEventType :: ToBrick -> e,
    fromAppEventType :: e -> Maybe ToBrick
  }

fromBrickChan :: Lens' (Config e) (TChan Event)
fromBrickChan = lens _fromBrickChan (\as x -> as { _fromBrickChan = x })


data Widget n = MkWidget
  { name :: n,
    visibleLines :: [String],
    hiddenLines :: [String],
    current :: (String, String),
    extent :: Maybe (Int, Int)
  }

visibleLinesL :: Lens' (Widget n) [String]
visibleLinesL = lens visibleLines (\w x -> w { visibleLines = x })


configure ::
  BC.BChan e ->
  (ToBrick -> e) ->
  (e -> Maybe ToBrick) ->
  IO (Config e)
configure toAppChan' toAppEventType' fromAppEventType' = do
  ch <- atomically $ newTChan
  return $
    MkConfig
      { _fromBrickChan = ch,
        toAppChan = toAppChan',
        toAppEventType = toAppEventType',
        fromAppEventType = fromAppEventType'
      }

initialWidget :: n -> String -> Widget n
initialWidget n st =
  MkWidget
    { name = n,
      visibleLines = [st],
      hiddenLines = [],
      current = ("", ""),
      extent = Nothing
    }

data ToBrick
  = LayoutRequest (MVar (Maybe Layout))
  | MoveToNextLine
  | PrintLines [String]
  | DrawLineDiff LineChars
  | ClearLayout

handleAppEvent ::
  (Eq n) =>
  Config e ->
  Widget n ->
  BrickEvent n e ->
  EventM n (Widget n)
handleAppEvent c w (AppEvent e) =
  case (fromAppEventType c) e of
    Just (LayoutRequest mv) -> do
      me <- lookupExtent (name w)
      case me of
        Just (Extent _ _ (wid, he) _) -> do
          liftIO . putMVar mv $ Just $ Layout wid he
          return $ w {extent = Just (wid, he)}
        Nothing -> do
          liftIO . putMVar mv $ Nothing
          return w
    Just MoveToNextLine -> do
      let (pre, suff) = current w
          w' =
            w
              { visibleLines = visibleLines w ++ [pre ++ suff],
                current = ("", "")
              }
      let vp = viewportScroll (name w)
      vScrollToEnd vp
      return w'
    Just (PrintLines ls) -> do
      return $ w {visibleLines = visibleLines w ++ ls}
    Just (DrawLineDiff (pre, suff)) -> do
      return $
        w
          { current =
              ( graphemesToString pre,
                graphemesToString suff
              )
          }
    Just ClearLayout -> do
      return $
        w
          { visibleLines = [],
            hiddenLines = hiddenLines w ++ visibleLines w
          }
    Nothing -> return w

handleEditorEvent :: TChan Event -> V.Event -> Widget n -> EventM n (Widget n)
handleEditorEvent c (V.EvKey k ms) w = do
  liftIO $ print $ show $ mkKeyEvent k
  liftIO $ atomically $ writeTChan c $ mkKeyEvent k
  return w
  where
    mkKeyEvent :: V.Key -> Event
    mkKeyEvent (V.KChar c') =
      KeyInput [addModifiers ms $ simpleKey (KeyChar c')]
    mkKeyEvent V.KEnter =
      KeyInput [addModifiers ms $ simpleKey (KeyChar '\n')]
    mkKeyEvent V.KBS =
      KeyInput [addModifiers ms $ simpleKey Backspace]
    mkKeyEvent V.KDel =
      KeyInput [addModifiers ms $ simpleKey Delete]
    mkKeyEvent V.KLeft =
      KeyInput [addModifiers ms $ simpleKey LeftKey]
    mkKeyEvent V.KRight =
      KeyInput [addModifiers ms $ simpleKey RightKey]
    mkKeyEvent V.KUp =
      KeyInput [addModifiers ms $ simpleKey UpKey]
    mkKeyEvent V.KDown =
      KeyInput [addModifiers ms $ simpleKey DownKey]
    mkKeyEvent _ = KeyInput []

    addModifiers :: [V.Modifier] -> Key -> Key
    addModifiers [] k' = k'
    addModifiers (V.MShift : tl) (Key m bc) =
      addModifiers tl $ (Key m {hasShift = True} bc)
    addModifiers (V.MCtrl : tl) (Key m (KeyChar c')) =
      addModifiers tl $ Key m (KeyChar $ setControlBits c')
    addModifiers (V.MCtrl : tl) k' = addModifiers tl . ctrlKey $ k'
    addModifiers (V.MMeta : tl) k' = addModifiers tl . metaKey $ k'
    addModifiers (V.MAlt : tl) k' = addModifiers tl k'

{-
handleEvent _ w (VtyEvent (V.EvResize _ _)) = do
  me <- lookupExtent (name w)
  case me of
    Just (Extent _ _ (wid, he) _) -> do
      return $ w {extent = Just (wid, he)}
    Nothing -> return w
handleEvent _ w _ = return w
 -}

useBrick :: Config e -> Behavior
useBrick c = Behavior (brickRunTerm c)

brickRunTerm :: Config e -> IO RunTerm
brickRunTerm c = do
  let tops =
        TermOps
          { getLayout = getLayout',
            withGetEvent = withGetEvent',
            saveUnusedKeys = saveKeys (_fromBrickChan c),
            evalTerm = evalBrickTerm c,
            externalPrint =
              atomically . writeTChan (_fromBrickChan c) . ExternalPrint
          }
  return $
    RunTerm
      { putStrOut = putStrOut',
        termOps = Left tops,
        wrapInterrupt = id,
        closeTerm = return ()
      }
  where
    putStrOut' :: String -> IO ()
    putStrOut' s = do
      BC.writeBChan (toAppChan c) $
        toAppEventType c $ PrintLines [s]

    getLayout' :: IO Layout
    getLayout' = do
      mv <- newEmptyMVar
      let e = toAppEventType c $ LayoutRequest mv
      BC.writeBChan (toAppChan c) e
      ml <- takeMVar mv
      case ml of
        Just l -> return $ l
        Nothing -> return $ Layout 0 0

    withGetEvent' ::
      forall m a.
      CommandMonad m =>
      (m Event -> m a) ->
      m a
    withGetEvent' f = f $ liftIO $ atomically $ readTChan (_fromBrickChan c)

newtype BrickTerm m a = MkBrickTerm {unBrickTerm :: ReaderT (ToBrick -> IO ()) m a}
  deriving
    ( MonadIO,
      MonadMask,
      MonadThrow,
      MonadCatch,
      Monad,
      Applicative,
      Functor,
      MonadReader (ToBrick -> IO ())
    )

instance MonadTrans BrickTerm where
  lift = MkBrickTerm . lift

evalBrickTerm :: (MonadReader Layout m, CommandMonad m) => Config e -> EvalTerm m
evalBrickTerm c =
  EvalTerm
    (flip runReaderT send . unBrickTerm)
    (MkBrickTerm . lift)
  where
    send = BC.writeBChan (toAppChan c) . toAppEventType c

instance
  (MonadMask m, MonadIO m, MonadReader Layout m) =>
  Term (BrickTerm m)
  where
  drawLineDiff _ d = sendToBrick $ DrawLineDiff d
  reposition _ _ = return ()
  printLines ls = sendToBrick $ PrintLines ls
  clearLayout = sendToBrick ClearLayout
  moveToNextLine _ = sendToBrick MoveToNextLine
  ringBell _ = return ()

sendToBrick :: (MonadReader Layout m, MonadIO m) => ToBrick -> BrickTerm m ()
sendToBrick e = do
  f <- System.Console.Haskeline.Internal.ask
  liftIO $ f e

render :: (Ord n, Show n) => Widget n -> B.Widget n
render
  ( MkWidget
      { name = n,
        current = (pre, suff),
        visibleLines = ls,
        extent = mext
      }
    ) =
    reportExtent n $ viewport n Vertical $ prev <=> curr
    where
      prev = vBox $ map str $ concat $ map (wrap mext) ls
      curr =
        visible $
          showCursor n (location mext) $
            vBox $ map str $ wrap mext $ pre ++ suff

      location Nothing = Location (length pre, 0)
      location (Just (w, _)) =
        let (q, r) = divMod (length pre) w
         in Location (r, q)

      wrap :: Maybe (Int, Int) -> String -> [String]
      wrap Nothing l = [l]
      wrap (Just (w, _)) l = go l
        where
          go xs = case splitAt w xs of
            (ys, []) -> [ys]
            (ys, tl) -> ys : go tl
