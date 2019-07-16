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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

{- |

This module provides a test framework for user acceptance testing
(UAT) of command line or console applications.  The framework
establishes <https://github.com/tmux/tmux/wiki tmux> sessions for
test cases and provides functions for sending input to the tmux
session and making assertions about the state of the terminal (i.e.
what is on the screen).

At a glance, the library works like this.  Test cases ('TestCase')
are defined using 'withTmuxSession'.  The 'testTmux' function groups
test cases into a /Tasty/ 'TestTree'.  Test cases are executed
sequentially, each in a separate tmux session.  Each test case can
have a setup and teardown action, and there can also be a setup and
teardown around the whole group of tests.

Let's look at a specific usage example.  You want to test some
program.  There are two tests.  Each test needs a dedicated
temporary directory, but they also need a separate, shared temporary
directory.  We want the setup routines to create these directories
and the teardown routines to remove them.  Assume the
existence of @mkTempDir :: IO FilePath@ and @rmDir :: FilePath
-> IO ()@.

Looking first at 'testTmux':

@
data SharedEnv = SharedEnv FilePath

myTests :: TestTree
myTests = 'testTmux' pre post [test1, test2]
  where
  pre = SharedEnv \<$\> mkTempDir
  post (SharedEnv path) = rmDir path
@

The shared setup action @pre@ returns a @SharedEnv@ value that will
be propagated to each test case, as well as the @teardown@ action,
after all test cases have run.

@test1@ and @test2@ are defined thus:

@
test1 :: TestCase SharedEnv
test1 = withTmuxSession setup teardown "putFile" $ \\step -> do
  -- test environment is availabe via 'ask'
  TestEnv _ sharedDir testDir <- 'ask'

  -- send a command to the tmux session and wait for \"Done\"
  'sendLine' ("myProg putFile " <> sharedDir) (Literal "Done.")

  -- save a snapshot of the terminal state and make some assertions
  'snapshot'
  'assertSubstringS' "The output should contain this substring"
  'assertRegexS' "The output should match this [Rr]eg[Ee]x"

test2 :: TestCase SharedEnv
test2 = withTmuxSession setup teardown "checkFile" $ \\step -> do
  TestEnv _ sharedDir testDir <- 'ask'

  -- use 'step' to label different stages of the test
  step "Run program"
  sendLine ("myProg checkFile " <> sharedDir) (Literal "Yep, it's there.")

  step "Check exit code"
  sendLine "echo status $?" (Literal "status 0")
@

Further discussion of the setup action is warranted.  This function,
at minimum, must incorporate the 'TmuxSession' argument into the
value it returns.  The type it returns must have an instance of
'HasTmuxSession'; this provides the name of the tmux session to the
framework functions that interact with tmux.

In our example, it also creates a per-test case temporary directory.
The value returned by the setup action is provided to the teardown
action.

@
data TestEnv = TestEnv
  { _session    :: 'TmuxSession'
  , _sharedDir  :: FilePath
  , _testDir    :: FilePath
  }

instance 'HasTmuxSession' TestEnv where
  'tmuxSession' = 'lens' _session (\\s b -> s { _session = b })

setup :: SharedEnv -> TmuxSession -> IO TestEnv
setup (SharedEnv sharedDir) session = TestEnv session sharedDir \<$\> mkTempDir

teardown :: TestEnv -> IO ()
teardown (TestEnv _ _ testDir) = rmDir testDir
@

If either shared or test-specific setup and teardown are not needed,
the 'testTmux'' and 'withTmuxSession'' functions are provided for
convenience.

-}
module UAT
  (
  -- * Creating test cases
    testTmux
  , testTmux'
  , withTmuxSession
  , withTmuxSession'
  , TestCase

  -- ** Test environment
  , HasTmuxSession(..)
  , TmuxSession

  -- * Sending input to a session
  , sendKeys
  , sendLiteralKeys
  , sendLine
  , tmuxSendKeys
  , TmuxKeysMode(..)
  , setEnvVarInSession

  -- * Capturing terminal state
  , capture
  , snapshot
  , Capture
  , captureString

  -- * Assertions
  , waitForCondition
  , Condition(..)
  , defaultRetries
  , defaultBackoff
  , assertCondition
  , assertSubstring
  , assertRegex

  -- ** State-aware assertions
  , assertConditionS
  , assertSubstringS
  , assertRegexS

  -- ** Helper functions
  , buildAnsiRegex

  -- * Re-exports
  , put

  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (catch, IOException)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO, MonadReader, runReaderT)
import Control.Monad.State (MonadState, get, put, runStateT)
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
import Test.Tasty.HUnit (assertBool, testCaseSteps)

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
-- Parameterised over the shared environment type.
type TestCase a = IO a -> Int -> TestTree

-- | tmux session name
type TmuxSession = String

-- | This class provides access to a tmux session name.  Test
-- environment types must have an instance of this class.
class HasTmuxSession a where
  -- | Lens to the 'TmuxSession'
  tmuxSession :: Lens' a TmuxSession

instance HasTmuxSession TmuxSession where
  tmuxSession = id


-- | Run a series of tests in tmux sessions.
--
-- Tests are executed sequentially.  Each test case is executed in a
-- new tmux session.  The name of the session is derived from the
-- name of the test and prepended with the sequence number.
--
-- A session called "keepalive" is created before any test cases are
-- run, and killed after all the test cases have finished.  This
-- session ensures that the tmux server remains alive, avoiding some race
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

-- | Like 'testTmux' but with no setup or teardown
testTmux' :: [TestCase ()] -> TestTree
testTmux' = testTmux (pure ()) (const $ pure ())


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
  case checkCondition cond (captureString cap) of
    True -> pure cap
    _ | n > 0 -> do
          liftIO $ threadDelay backOff
          waitForCondition cond (n - 1) (backOff * 4)
      | otherwise -> cap <$ assertCondition cond cap

checkCondition :: Condition -> String -> Bool
checkCondition Unconditional = const True
checkCondition (Literal s) = (s `isInfixOf`)
checkCondition (Regex re) = (=~ re)

-- | Assert that the capture satisfies a condition
assertCondition :: (MonadIO m) => Condition -> Capture -> m ()
assertCondition cond cap =
  let s = captureString cap
  in liftIO $ assertBool
    ( "Condition not met: '" <> show cond
    <> "'.  Last capture:\n\n " <> s <> "\n\n" <> " raw: " <> show s )
    (checkCondition cond s)

-- | Substring assertion.
assertSubstring :: (MonadIO m) => String -> Capture -> m ()
assertSubstring = assertCondition . Literal

-- | Regex assertion.
assertRegex :: (MonadIO m) => String -> Capture -> m ()
assertRegex = assertCondition . Regex

-- | Assert that the saved capture satisfies a condition.
--
-- Use 'snapshot' to save a capture:
--
-- @
-- snapshot
-- assertConditionS (Regex "[Ff][Oo][Oo]")
-- @
--
-- Alternatively, use 'put' on the result of any action that returns
-- a 'Capture':
--
-- @
-- 'sendKeys' "Enter" Unconditional >>= 'put'
-- assertConditionS (Literal "Doing thing...")
-- @
--
-- See also 'assertSubstringS' and 'assertRegexS'.
--
assertConditionS :: (MonadIO m, MonadState Capture m) => Condition -> m ()
assertConditionS cond = get >>= assertCondition cond

-- | State-aware substring assertion.
assertSubstringS :: (MonadIO m, MonadState Capture m) => String -> m ()
assertSubstringS s = get >>= assertSubstring s

-- | State-aware regex assertion.
assertRegexS :: (MonadIO m, MonadState Capture m) => String -> m ()
assertRegexS s = get >>= assertRegex s

-- | 5
defaultRetries :: Int
defaultRetries = 5

-- | Run all application steps in a session defined by session name.
withTmuxSession
  :: (HasTmuxSession testEnv)
  => (sharedEnv -> TmuxSession -> IO testEnv)
  -- ^ Set up session.  The tmux session is established before this
  -- action is run.  Takes the shared environment and Tmux session
  -- and constructs a test environment value (which must make the
  -- 'TmuxSession' available via its 'HasTmuxSession' instance).
  -> (testEnv -> IO ())
  -- ^ Tear down the session.  The tmux session will be torn down
  -- /after/ this action.
  -> TestName
  -- ^ Name of the test (a string).
  -> ( forall m. (MonadReader testEnv m, MonadState Capture m, MonadIO m)
       => (String -> m ()) -> m a
     )
  -- ^ The main test function.  The argument is the "step" function
  -- which can be called with a description to label the steps of
  -- the test procedure.
  -> TestCase sharedEnv
withTmuxSession pre post desc f getGEnv i =
  withResource
    (getGEnv >>= \gEnv -> frameworkPre >>= pre gEnv)
    (\env -> post env *> cleanUpTmuxSession (view tmuxSession env))
    $ \env -> testCaseSteps desc $
        \step -> env >>= void . runReaderT (runStateT (f (liftIO . step)) initCap)
  where
    initCap = error "no Capture; use 'snapshot' first"
    frameworkPre =
      let
        -- FIXME? customisable session name prefix?
        sessionName = intercalate "-" ("tasty-tmux" : show i : descWords)
        descWords = words $ filter (\c -> isAscii c && (isAlphaNum c || c == ' ')) desc
      in
        setUpTmuxSession sessionName

-- | Like 'withTmuxSession' but without setup and teardown.  Shared
-- environment value (and its type) is ignored.
withTmuxSession'
  :: TestName
  -> ( forall m. (MonadReader TmuxSession m, MonadState Capture m, MonadIO m)
       => (String -> m ()) -> m a
     )
  -- ^ The main test function.  The argument is the "step" function
  -- which can be called with a description to label the steps of
  -- the test procedure.
  -> TestCase sharedEnv
withTmuxSession' = withTmuxSession (const pure) (const $ pure ())

-- | Capture the current terminal state.
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

-- | Snapshot the current terminal state.
--
-- @
-- snapshot = 'capture' >>= 'put'
-- @
--
-- Use functions like 'assertConditionS' to make assertions on the
-- most recent snapshot.
--
snapshot :: (HasTmuxSession a, MonadReader a m, MonadState Capture m, MonadIO m) => m ()
snapshot = capture >>= put

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
             , "tasty-tmux"])
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
