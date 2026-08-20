-- copied from https://github.com/rootmos/haskeline/blob/master/System/Console/Haskeline/Brick.hs
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE FunctionalDependencies #-}

module Brick.Haskeline
  ( newWidget
  , withHaskeline
  , HasHaskelineEvent(..)
  , Widget
  , Config
  , ToBrick(..)
  , tbNameL
  , handleEvent
  , handleEditorEvent
  , handleAppEvent
  , submitLineSync
  , render
  , fromBrickChan
  , useBrick
  , contentsL
  , configL
  , nameL
  , submittedL
  , lastSubmittedL
  , clearLine
  , clearLineWithSeed
  , setLine
  )
where

import Brick hiding (Widget, render)
import qualified Brick as B
import qualified Brick.BChan as BC
import GHC.Conc (atomically)
import Data.Foldable (for_)
import qualified Data.Text.Zipper as Z hiding (textZipper)
import qualified Data.Text.Zipper.Generic as Z
import qualified Control.Monad.Reader as MTL
import Control.Concurrent
import Control.Concurrent.STM (TVar, TMVar, TChan, readTChan, newTChan, writeTChan, tryTakeTMVar, takeTMVar, newTVarIO, newEmptyTMVarIO, readTVarIO, writeTVar, putTMVar)
import Control.Monad.Loops (whileJust_)
import Control.Concurrent.Async (withAsync)
import Control.Lens (Prism', Lens', lens, view, use, review, preview, modifying)
import qualified Graphics.Vty as V
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Catch (MonadMask, MonadCatch, MonadThrow)
import Control.Monad.Trans.Class (MonadTrans(..))
import System.Console.Haskeline (runInputTBehavior, getInputLineWithInitial, Settings)
import System.Console.Haskeline.Internal (
  Event(..), Key (..), BaseKey(KeyChar), Term(..), TermOps(..), hasShift,
  simpleKey, Layout(..), LineChars, BaseKey(..), RunTerm(..),
  CommandMonad, EvalTerm(..), setControlBits, ctrlKey,
  metaKey, saveKeys, graphemesToString, MonadReader(..), Behavior(..))

data ToBrick n = MkToBrick
  { tbName :: n
  , tbPayload :: ToBrickPayload
  }

tbNameL :: Lens' (ToBrick n) n
tbNameL = lens tbName (\tb x -> tb { tbName = x })


data ToBrickPayload
  = LayoutRequest (MVar (Maybe Layout))
  | MoveToNextLine
  | PrintLines !String
  | DrawLineDiff LineChars
  | ClearLayout
  | LineSubmitted String

class HasHaskelineEvent n e | e -> n where
  _HaskelineEvent :: Prism' e (ToBrick n)

data Config n e = MkConfig
  { fromBrickChan :: TChan Event
  , toAppChan :: BC.BChan e
  , toAppEventType :: ToBrick n -> e
  , fromAppEventType :: e -> Maybe (ToBrick n)
  , configName :: n
  }

fromBrickChanL :: Lens' (Config n e) (TChan Event)
fromBrickChanL = lens fromBrickChan (\as x -> as { fromBrickChan = x })

data Widget n e = MkWidget
  { name :: n
  , current :: (String, String)
  , extent :: Maybe (Int, Int)
  , config :: Config n e
  , submitted :: TMVar String -- ^ the submitted query string
  , lastSubmitted :: Maybe String
  , initialText :: TVar String -- ^ initial text seed
  }

contentsL :: Lens' (Widget n e) (Z.TextZipper String)
contentsL = lens getter setter
  where
    getter w =
      let (pre, suff) = current w
      in  Z.moveCursor (0, length pre)
            (Z.textZipper [pre ++ suff] (Just 1))

    setter w z =
      let line   = Z.currentLine z
          (_, c) = Z.cursorPosition z
          (pre, suff) = splitAt c line
      in  w { current = (pre, suff) }

currentL :: Lens' (Widget n e) (String, String)
currentL = lens current (\w x -> w { current = x })

extentL :: Lens' (Widget n e) (Maybe (Int, Int))
extentL = lens extent (\w x -> w { extent = x })

configL :: Lens' (Widget n e) (Config n e)
configL = lens config (\w x -> w { config = x })

nameL :: Lens' (Widget n e) n
nameL = lens name (\w x -> w { name = x })

submittedL :: Lens' (Widget n e) (TMVar String)
submittedL = lens submitted (\w x -> w { submitted = x })

lastSubmittedL :: Lens' (Widget n e) (Maybe String)
lastSubmittedL = lens lastSubmitted (\w x -> w { lastSubmitted = x })

newWidget
  :: (HasHaskelineEvent n e)
  => BC.BChan e
  -> n
  -> String  -- initial text
  -> IO (Widget n e)
newWidget chan n seed = do
  fromBrick <- atomically newTChan
  submit <- newEmptyTMVarIO
  cancel <- newEmptyTMVarIO
  seedVar <- newTVarIO seed

  let cfg = MkConfig
        { fromBrickChan = fromBrick
        , toAppChan = chan
        , toAppEventType = review _HaskelineEvent
        , fromAppEventType = preview _HaskelineEvent
        , configName = n
        }

  pure MkWidget
    { name = n
    , current = (seed, "")
    , extent = Nothing
    , config = cfg
    , submitted = submit
    , lastSubmitted = Nothing
    , initialText = seedVar
    }

withHaskeline
  :: (HasHaskelineEvent n e)
  => BC.BChan e
  -> n
  -> String
  -> Settings IO
  -> (Widget n e -> IO a)
  -> IO a
withHaskeline chan n initial settings k = do
  w <- newWidget chan n initial
  withAsync (runWidget w settings) $ \_ -> k w

handleEvent :: (HasHaskelineEvent n e, Eq n) => BrickEvent n e -> EventM n (Widget n e) ()
handleEvent (VtyEvent ev) = handleEditorEvent ev
handleEvent ev@(AppEvent _) = handleAppEvent ev
handleEvent _ = pure ()

handleAppEvent ::
  (HasHaskelineEvent n e, Eq n) =>
  BrickEvent n e ->
  EventM n (Widget n e) ()
handleAppEvent (AppEvent e) = do
  name <- use nameL
  case preview _HaskelineEvent e of
    Just (MkToBrick n p) | n == name -> handlePayload p
    _ -> pure ()
handleAppEvent _ = pure ()

handlePayload (LayoutRequest mv) = do
      w <- get
      me <- lookupExtent (name w)
      case me of
        Just (Extent _ _ (wid, he)) -> do
          liftIO . putMVar mv $ Just $ Layout wid he
          put $ w {extent = Just (wid, he)}
        Nothing -> do
          liftIO . putMVar mv $ Nothing
          put w
handlePayload MoveToNextLine = pure ()
handlePayload (PrintLines _) = pure ()
handlePayload (DrawLineDiff (pre, suff)) =
      modifying currentL (const ( graphemesToString pre,
               graphemesToString suff
             ))
handlePayload ClearLayout = modifying currentL (const ("", ""))
handlePayload (LineSubmitted s) = modifying lastSubmittedL (const (Just s))

mkKeyEventMaybe :: V.Key -> [V.Modifier] -> Maybe Event
mkKeyEventMaybe (V.KChar 'u') ms
    | V.MCtrl `elem` ms =
        Just $ KeyInput [ simpleKey (KeyChar (setControlBits 'a'))
                        , simpleKey (KeyChar (setControlBits 'k'))
                        ]
mkKeyEventMaybe (V.KChar c') ms =
    Just $ KeyInput [addModifiers ms $ simpleKey (KeyChar c')]
mkKeyEventMaybe V.KEnter ms =
    Just $ KeyInput [addModifiers ms $ simpleKey (KeyChar '\n')]
mkKeyEventMaybe V.KBS ms =
    Just $ KeyInput [addModifiers ms $ simpleKey Backspace]
mkKeyEventMaybe V.KDel ms =
    Just $ KeyInput [addModifiers ms $ simpleKey Delete]
mkKeyEventMaybe V.KLeft ms =
    Just $ KeyInput [addModifiers ms $ simpleKey LeftKey]
mkKeyEventMaybe V.KRight ms =
    Just $ KeyInput [addModifiers ms $ simpleKey RightKey]
mkKeyEventMaybe V.KUp ms =
    Just $ KeyInput [addModifiers ms $ simpleKey UpKey]
mkKeyEventMaybe V.KDown ms =
    Just $ KeyInput [addModifiers ms $ simpleKey DownKey]
mkKeyEventMaybe V.KEsc _ = Just $ KeyInput [simpleKey (KeyChar '\ESC')]
mkKeyEventMaybe _ _ = Nothing

addModifiers :: [V.Modifier] -> Key -> Key
addModifiers [] k' = k'
addModifiers (V.MShift : tl) (Key m bc) =
    addModifiers tl $ Key m {hasShift = True} bc
addModifiers (V.MCtrl : tl) (Key m (KeyChar c')) =
    addModifiers tl $ Key m (KeyChar $ setControlBits c')
addModifiers (V.MCtrl : tl) k' = addModifiers tl . ctrlKey $ k'
addModifiers (V.MMeta : tl) k' = addModifiers tl . metaKey $ k'
addModifiers (V.MAlt : tl) k' = addModifiers tl k'

handleEditorEvent :: Eq n => V.Event -> EventM n (Widget n e) ()
handleEditorEvent (V.EvResize _ _) = do
  n <- use nameL
  me <- lookupExtent n
  case me of
    Just (Extent _ _ dims) -> modifying extentL (const (Just dims))
    Nothing -> return ()
handleEditorEvent (V.EvKey k ms) = do
  ch <- use (configL . fromBrickChanL)
  for_ (mkKeyEventMaybe k ms) (liftIO . atomically . writeTChan ch)
handleEditorEvent _ = pure ()

useBrick :: Config n e -> Behavior
useBrick c = Behavior (brickRunTerm c)

brickRunTerm :: Config n e -> IO RunTerm
brickRunTerm c = do
  let tops =
        TermOps
          { getLayout = getLayout',
            withGetEvent = withGetEvent',
            saveUnusedKeys = saveKeys (fromBrickChan c),
            evalTerm = evalBrickTerm c,
            externalPrint =
              atomically . writeTChan (fromBrickChan c) . ExternalPrint
          }
  return $
    RunTerm
      { putStrOut = putStrOut',
        termOps = Left tops,
        wrapInterrupt = id,
        closeTerm = return ()
      }
  where
    tag = MkToBrick (configName c)
    putStrOut' :: String -> IO ()
    putStrOut' s = BC.writeBChan (toAppChan c) $ toAppEventType c $ tag $ PrintLines s

    getLayout' :: IO Layout
    getLayout' = do
      mv <- newEmptyMVar
      let e = toAppEventType c $ tag $ LayoutRequest mv
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
    withGetEvent' f = f $ liftIO $ atomically $ readTChan (fromBrickChan c)

newtype BrickTerm n m a = MkBrickTerm {unBrickTerm :: ReaderT (n, ToBrick n -> IO ()) m a}
  deriving
    ( MonadIO,
      MonadMask,
      MonadThrow,
      MonadCatch,
      Monad,
      Applicative,
      Functor,
      MonadReader (n, ToBrick n -> IO ())
    )

instance MonadTrans (BrickTerm n) where
  lift = MkBrickTerm . lift

evalBrickTerm :: CommandMonad m => Config n e -> EvalTerm m
evalBrickTerm c =
  EvalTerm
    (flip runReaderT (configName c, send) . unBrickTerm)
    (MkBrickTerm . lift)
  where
    send = BC.writeBChan (toAppChan c) . toAppEventType c

instance
  (MonadMask m, MonadIO m, MonadReader Layout m) =>
  Term (BrickTerm n m)
  where
  drawLineDiff _ d = sendToBrick $ DrawLineDiff d
  reposition _ d = sendToBrick $ DrawLineDiff d
  moveToNextLine _ = sendToBrick MoveToNextLine
  printLines ls = sendToBrick $ PrintLines (concat ls)
  clearLayout = sendToBrick ClearLayout
  ringBell _ = return ()

askEnv :: Monad m => BrickTerm n m (n, ToBrick n -> IO ())
askEnv = MkBrickTerm MTL.ask

sendToBrick :: MonadIO m => ToBrickPayload -> BrickTerm n m ()
sendToBrick p = do
  (n, f) <- askEnv
  liftIO (f (MkToBrick n p))


submitLineSync :: Widget n e -> IO String
submitLineSync w = do
  let sub = view submittedL w
  _ <- atomically $ tryTakeTMVar sub
  submitLine w
  atomically $ takeTMVar sub

-- submit the line to Haskel. This is useful if Brick handles the action and you need to make sure Haskeline finalizes
submitLine :: Widget n e -> IO ()
submitLine = atomically . flip writeTChan ev . view (configL . fromBrickChanL)
  where ev = KeyInput [simpleKey (KeyChar '\n')]

render :: Ord n => Widget n e -> B.Widget n
render
  ( MkWidget
      { name = n,
        current = (pre, suff),
        extent = mext
      }
    ) =
  let line = pre ++ suff
      cursor = length pre
      -- Add scrolling behaviour if Term is too small
      width = fmap fst mext
      offset = case width of
        Just w | cursor >= w -> max 0 (cursor - w + 1)
        Just _ -> 0
        Nothing -> 0
      visibleLine = drop offset line
      cursorCol = cursor - offset
  in
    reportExtent n
    $ vLimit 1
    $ padRight Max
    $ showCursor n (Location (cursorCol, 0))
    $ str visibleLine

runWidget :: Widget n e -> Settings IO -> IO ()
runWidget w settings =
  runInputTBehavior (useBrick (config w)) settings loop
  where
    loop = whileJust_ readline processLine
    readline = do
      liftIO (readTVarIO (initialText w))
      >>= \s -> getInputLineWithInitial "" (s, "")
    processLine s = do
      liftIO $ atomically $ do
        writeTVar (initialText w) s
        _ <- tryTakeTMVar (submitted w)
        putTMVar (submitted w) s
      liftIO $ BC.writeBChan (toAppChan (config w))
             $ toAppEventType (config w)
             $ MkToBrick (configName (config w)) (LineSubmitted s)

-- | Reset the current line to empty, without tearing down the
-- background input loop. Clears the seed used for the next line and
-- tells the running Haskeline session to kill the current line.
-- Note: Beware to read the current field right after issuing a
-- clearLine. The line clearance is processed on the next event tick
-- and not necessarily immediately.
clearLine :: Widget n e  -> IO ()
clearLine = clearLineWithSeed ""

clearLineWithSeed :: String -> Widget n e -> IO ()
clearLineWithSeed s w = do
  atomically $ writeTVar (initialText w) s
  setLine s w

setLine :: String -> Widget n e -> IO ()
setLine s w = atomically $ do
  let ch = view (configL . fromBrickChanL) w
  -- move to end, kill the entire line
  writeTChan ch (KeyInput [ctrlKey (simpleKey (KeyChar 'e'))])
  writeTChan ch (KeyInput [ctrlKey (simpleKey (KeyChar 'U'))])
  -- literally type in the seed
  writeTChan ch (KeyInput (map (simpleKey . KeyChar) s))
