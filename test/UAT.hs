-- This file is part of purebred
-- Copyright (C) 2017-2019 Róman Joost and Fraser Tweedale
--
-- purebred is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE RankNTypes #-}

module UAT
  (
  -- * Creating tmux test cases
    testTmux
  , withTmuxSession
  , TestCase

  -- ** Session environment
  , HasTmuxSession(..)
  , TmuxSession

  -- * Captures
  , capture
  , Capture
  , captureString

  -- * Assertions
  , waitForCondition
  , Condition(..)
  , defaultRetries
  , defaultBackoff
  , buildAnsiRegex
  , (=~)

  -- * Sending input to a session
  , sendKeys
  , sendLiteralKeys
  , sendLine
  , tmuxSendKeys
  , TmuxKeysMode(..)
  , setEnvVarInSession

  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (catch, IOException)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO, MonadReader, runReaderT)
import qualified Data.ByteString.Lazy as L
import Data.Char (isAscii, isAlphaNum)
import Data.List (intercalate, isInfixOf)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import System.IO (hPutStrLn, stderr)

import Control.Lens (Lens', view)
import System.Process.Typed
  ( proc, runProcess_, readProcessInterleaved_, ProcessConfig )
import Text.Regex.Posix ((=~))

import Test.Tasty (TestTree, TestName, testGroup, withResource)
import Test.Tasty.HUnit (assertFailure, testCaseSteps)

-- | A condition to check for in the output of the program
data Condition
  = Unconditional
  | Literal String
  | Regex String
  deriving (Show)

-- | A captured pane.  For now this just contains the string content,
-- but in the future perhaps we will augment it with terminal title,
-- terminal dimensions, timestamp, etc.
--
-- Use 'captureString' to get at the string.
--
newtype Capture = Capture { _captureString :: String }

-- | Get the captured terminal content.
captureString :: Capture -> String
captureString = _captureString

-- | A test case that will be executed in a dedicated tmux session.
-- Parameterised over the "global" environment (c.f. the "session"
-- environment).
type TestCase a = IO a -> Int -> TestTree

type TmuxSession = String

class HasTmuxSession a where
  tmuxSession :: Lens' a TmuxSession


-- | Run a series of tests in tmux sessions.
--
-- Tests are executed sequentially.  Each test case is executed in a
-- new tmux session.  The name of the session is derived from the
-- name of the test and prepended with the sequence number.
--
-- A session called "keepalive" is created before any test cases are
-- run, and killed after all the test cases have finished.  This
-- session ensures that the server remains alive, avoiding some race
-- conditions.
--
testTmux
  :: IO a
  -- ^ Set-up action.  Executed one time, after the keepalive
  -- session is created but before any test cases are executed.
  -> (a -> IO ())
  -- ^ Tear-down action.  Executed after all test cases have
  -- finished but before the keepalive session gets killed.
  -> [TestCase a]
  -> TestTree
testTmux pre post tests =
  withResource (frameworkPre *> pre) (\a -> post a *> frameworkPost) $ \env ->
    testGroup "user acceptance tests" $ zipWith ($ env) tests [0..]
  where
    keepaliveSessionName = "keepalive"
    frameworkPre = setUpTmuxSession keepaliveSessionName
    frameworkPost = cleanUpTmuxSession keepaliveSessionName


type AnsiAttrParam = String
type AnsiFGParam = String
type AnsiBGParam = String

-- | Generate a regex for an escape sequence setting the given
-- foreground and background parameters
--
-- tmux < 03d01eabb5c5227f56b6b44d04964c1328802628 (first released
-- in tmux-2.5) ran attributes, foreground colour and background
-- colour params separated by semicolons (foreground first).
--
-- After that commit, attributes, foreground colours and background
-- colours are written in separate escape sequences.  Therefore for
-- compatibility with different versions of tmux there are two
-- patterns to check.
--
buildAnsiRegex :: [AnsiAttrParam] -> [AnsiFGParam] -> [AnsiBGParam] -> String
buildAnsiRegex attrs fgs bgs =
  let
    withSemis = intercalate ";"
    wrap [] = ""
    wrap xs = "\ESC\\[" <> withSemis xs <> "m"
    tmux24 = wrap (attrs <> fgs <> bgs)
    tmux25 = wrap attrs <> wrap fgs <> wrap bgs
    choice "" "" = ""
    choice "" r = r
    choice l "" = l
    choice l r = "(" <> l <> "|" <> r <> ")"
  in
    choice tmux24 tmux25

-- | Sets a shell environment variable.
--
-- Note: The tmux program provides a command to set environment variables for
-- running sessions, yet they seem to be not inherited by the shell.
--
-- This assumes that a standard shell prompt is ready in the session.
-- No attempt is made to check this; it just blindly send they keystrokes.
--
setEnvVarInSession
  :: (HasTmuxSession a, MonadReader a m, MonadIO m)
  => String -> String -> m ()
setEnvVarInSession name value =
  void $ sendLine ("export " <> name <> "=" <> value) Unconditional

-- | Send interpreted keys into the program and wait for the
-- condition to be met, failing the test if the condition is not met
-- after some time.
sendKeys
  :: (HasTmuxSession a, MonadReader a m, MonadIO m)
  => String -> Condition -> m Capture
sendKeys keys expect = do
    tmuxSendKeys InterpretKeys keys
    waitForCondition expect defaultRetries defaultBackoff

-- | Send literal keys to the terminal and wait for the condition to
-- be satisfied, with default timeout.
sendLiteralKeys
  :: (HasTmuxSession a, MonadReader a m, MonadIO m)
  => String -> Condition -> m Capture
sendLiteralKeys keys cond = do
    tmuxSendKeys LiteralKeys keys
    waitForCondition cond defaultRetries defaultBackoff

-- | Send the literal string to the terminal, followed by @Enter@,
-- then wait for the condition be satisfied, with default timeout.
sendLine
  :: (HasTmuxSession a, MonadReader a m, MonadIO m)
  => String -> Condition -> m Capture
sendLine s cond = do
  void $ sendLiteralKeys s Unconditional
  sendKeys "Enter" cond

-- | Whether to tell tmux to treat keys literally or interpret
-- sequences like "Enter" or "C-x".
--
data TmuxKeysMode = LiteralKeys | InterpretKeys
  deriving (Eq)

-- | Send keystrokes into a tmux session.
--
tmuxSendKeys
  :: (HasTmuxSession a, MonadReader a m, MonadIO m)
  => TmuxKeysMode -> String -> m ()
tmuxSendKeys mode keys = tmuxSendKeysProc mode keys >>= runProcess_

-- | Construct the 'ProcessConfig' for a tmux command.  The session
-- name is read from the 'MonadReader' environment.
--
tmuxSendKeysProc
  :: (HasTmuxSession a, MonadReader a m)
  => TmuxKeysMode -> String -> m (ProcessConfig () () ())
tmuxSendKeysProc mode keys = tmuxSessionProc "send-keys" (["-l" | mode == LiteralKeys] <> [keys])

-- | Create a 'ProcessConfig' for a tmux command, taking the session
-- name from the 'MonadReader' environment.
--
tmuxSessionProc
  :: (HasTmuxSession a, MonadReader a m)
  => String -> [String] -> m (ProcessConfig () () ())
tmuxSessionProc cmd args = do
  sessionName <- view tmuxSession
  pure $ proc "tmux" (cmd : "-t" : sessionName : args)

-- | Capture the pane and check for a condition, optionally retrying
-- with exponential backoff.  If the condition is not met after the
-- final attempt, the test fails.
waitForCondition
  :: (HasTmuxSession a, MonadReader a m, MonadIO m)
  => Condition
  -> Int  -- ^ Number of retries allowed
  -> Int  -- ^ Initial microseconds to back off.  Multiplied by 4 on each retry.
  -> m Capture  -- ^ Return the successful capture (or throw an exception)
waitForCondition cond n backOff = do
  cap <- capture
  let s = captureString cap
  case checkCondition cond (captureString cap) of
    True -> pure cap
    _ | n > 0 -> do
          liftIO $ threadDelay backOff
          waitForCondition cond (n - 1) (backOff * 4)
      | otherwise -> liftIO $ assertFailure
          ( "Wait time exceeded. Condition not met: '" <> show cond
            <> "' last screen shot:\n\n " <> s <> "\n\n" <> " raw: " <> show s )

checkCondition :: Condition -> String -> Bool
checkCondition Unconditional = const True
checkCondition (Literal s) = (s `isInfixOf`)
checkCondition (Regex re) = (=~ re)

-- | 5
defaultRetries :: Int
defaultRetries = 5

-- | Run all application steps in a session defined by session name.
withTmuxSession
  :: (HasTmuxSession sessionEnv)
  => (globalEnv -> TmuxSession -> IO sessionEnv)
  -- ^ Set up session.  The tmux session is established before this
  -- action is run.  Takes the global environment and Tmux session
  -- and constructs a session environment value (which must make the
  -- 'TmuxSession' available via its 'HasTmuxSession' instance).
  -> (sessionEnv -> IO ())
  -- ^ Tear down the session.  The tmux session will be torn down
  -- /after/ this action.
  -> TestName
  -- ^ Name of the test (a string).
  -> (forall m. (MonadReader sessionEnv m, MonadIO m) => (String -> m ()) -> m a)
  -- ^ The main test function.  The argument is the "step" function
  -- which can be called with a description to label the steps of
  -- the test procedure.
  -> TestCase globalEnv
withTmuxSession pre post desc f getGEnv i =
  withResource
    (getGEnv >>= \gEnv -> frameworkPre >>= pre gEnv)
    (\env -> post env *> cleanUpTmuxSession (view tmuxSession env))
    $ \env -> testCaseSteps desc $
        \step -> env >>= runReaderT (void $ f (liftIO . step))
  where
    frameworkPre =
      let
        -- FIXME? customisable session name prefix?
        sessionName = intercalate "-" ("tasty-tmux" : show i : descWords)
        descWords = words $ filter (\c -> isAscii c && (isAlphaNum c || c == ' ')) desc
      in
        setUpTmuxSession sessionName

capture :: (HasTmuxSession a, MonadReader a m, MonadIO m) => m Capture
capture = Capture . T.unpack . decodeLenient . L.toStrict
  <$> (tmuxSessionProc "capture-pane"
    [ "-e"  -- include escape sequences
    , "-p"  -- send output to stdout
    , "-J"  -- join wrapped lines and preserve trailing whitespace
    ]
  >>= liftIO . readProcessInterleaved_)
  where
    decodeLenient = T.decodeUtf8With T.lenientDecode

-- | 20 milliseconds
defaultBackoff :: Int
defaultBackoff = 20 * 10 ^ (3 :: Int)

-- | create a tmux session running in the background
-- Note: the width and height are the default values tmux uses, but I thought
-- it's better to be explicit.
--
-- Returns the session name (whatever the input was) for convenience.
setUpTmuxSession :: TmuxSession -> IO TmuxSession
setUpTmuxSession sessionname = sessionname <$
    catch
        (runProcess_ $ proc
             "tmux"
             [ "new-session"
             , "-x"
             , "80"
             , "-y"
             , "24"
             , "-d"
             , "-s"
             , sessionname
             , "-n"
             , "purebred"])
        (\e ->
              do let err = show (e :: IOException)
                 hPutStrLn stderr ("\nException during setUp: " <> err)
                 pure ())

-- | Kills the whole session including pane and application
cleanUpTmuxSession :: String -> IO ()
cleanUpTmuxSession sessionname =
    catch
        (runProcess_ $ proc "tmux" ["kill-session", "-t", sessionname])
        (\e ->
              do let err = show (e :: IOException)
                 hPutStrLn stderr ("\nException when killing session: " <> err)
                 pure ())
