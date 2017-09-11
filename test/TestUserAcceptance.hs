{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
module TestUserAcceptance where

import qualified Data.Text as T
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Data.Ini (parseIni, writeIniFileWith, KeySeparator(..), WriteIniSettings(..))
import Data.Semigroup ((<>))
import Control.Concurrent
       (newEmptyMVar, putMVar, takeMVar, MVar, threadDelay)
import Control.Exception (catch, IOException)
import System.IO (hPutStr, stderr)
import System.Environment (lookupEnv)
import Control.Monad (void, when)
import Data.Maybe (isJust)

import Data.List (isInfixOf)
import System.Process (callProcess, readProcess)
import System.Directory
       (getCurrentDirectory, removeDirectoryRecursive)
import Test.Tasty (TestTree, testGroup, withResource, mkTimeout, localOption)
import Test.Tasty.HUnit (testCaseSteps, assertBool, Assertion)
import Text.Regex.Posix ((=~))

systemTests ::
  TestTree
systemTests =
    localOption (mkTimeout testTimeout) $
    testGroup
        "user acceptance tests"
        [ testUserViewsMailSuccessfully
        , testUserCanManipulateNMQuery
        , testUserCanSwitchBackToIndex
        , testCanToggleHeaders
        , testSetsMailToRead]

-- | maximum amount of time we allow a step to run until we fail it
-- 6 seconds should be plenty
testTimeout :: Integer
testTimeout = 10 ^ 6 * 8


testSetsMailToRead ::
  TestTree
testSetsMailToRead = tmuxSession "user can toggle read tag" steps
  where steps =
          [ApplicationStep
             ""
             "is unread (bold)"
             False
             "is Purebred"
             (\o _ ->
                assertBool "regex doesn't match out" $
                o =~ ("\ESC\\[1;.*Testmail" :: String))
          ,ApplicationStep "Enter" "views mail" False "This is a test mail" assertSubstrInOutput
          ,ApplicationStep
             "Escape"
             "is set to read"
             False
             "is Purebred"
             (\o _ ->
                assertBool "regex doesn't match out" $
                o =~ ("\ESC\\[37.*Testmail" :: String))
          ,ApplicationStep
             "t"
             "toggled back to unread"
             False
             "1;37;43m" -- wait for the screen turns bold
             (\o _ ->
                assertBool "regex doesn't match out" $
                o =~ ("\ESC\\[1;.*Testmail" :: String))]


testCanToggleHeaders ::
  TestTree
testCanToggleHeaders = tmuxSession "user can toggle Headers" steps
  where
    steps =
        [ ApplicationStep
              "Enter"
              "view mail"
              False
              "This is a test mail"
              assertSubstrInOutput
        , ApplicationStep
              "h"
              "show all headers"
              False
              "return-path"
              assertSubstrInOutput
        , ApplicationStep
              "h"
              "filtered headers"
              False
              "This is a test mail"
              (\o _ ->
                    assertBool "regex matches out" $
                    o =~ ("Purebred.*\n.*from" :: String))]

testUserViewsMailSuccessfully ::
  TestTree
testUserViewsMailSuccessfully = tmuxSession "user can view mail" steps
  where
    steps =
        [ ApplicationStep "" "shows tag" False "inbox" assertSubstrInOutput
        , ApplicationStep
              "Enter"
              "view mail"
              False
              "This is a test mail"
              assertSubstrInOutput]

testUserCanManipulateNMQuery ::
  TestTree
testUserCanManipulateNMQuery =
    tmuxSession
        "manipulating notmuch search query results in empty index"
        steps
  where
    steps =
        [ ApplicationStep
              ":"
              "focus command"
              False
              "37;40mtag"
              assertSubstrInOutput
        , ApplicationStep
              "C-u"
              "delete all input"
              False
              "37;40m"
              assertSubstrInOutput
        , ApplicationStep
              "tag:replied"
              "enter new tag"
              True
              "tag:replied"
              assertSubstrInOutput
        , ApplicationStep
              "Enter"
              "apply"
              False
              "Item 0 of 1"
              assertSubstrInOutput
        , ApplicationStep
              "Enter"
              "view current mail"
              False
              "HOLY PUREBRED"
              assertSubstrInOutput]

testUserCanSwitchBackToIndex ::
  TestTree
testUserCanSwitchBackToIndex =
    tmuxSession "user can switch back to mail index during composition" steps
  where
    steps =
        [ ApplicationStep
              "m"
              "start composition"
              False
              "From"
              assertSubstrInOutput
        , ApplicationStep
              "testuser@foo.test\r"
              "enter from email"
              False
              "To"
              assertSubstrInOutput
        , ApplicationStep
              "user@to.test\r"
              "enter to: email"
              False
              "Subject"
              assertSubstrInOutput
        , ApplicationStep
              "test subject\r"
              "enter subject"
              False
              "~"
              assertSubstrInOutput
        , ApplicationStep
              "iThis is a test body"
              "enter mail body"
              False
              "body"
              assertSubstrInOutput
        , ApplicationStep
              "Escape"
              "exit insert mode in vim"
              False
              "body"
              assertSubstrInOutput
        , ApplicationStep
              ": x\r"
              "exit vim"
              False
              "Attachments"
              assertSubstrInOutput
        , ApplicationStep
              "Tab"
              "switch back to index"
              False
              "Testmail"
              assertSubstrInOutput
        , ApplicationStep
              "Tab"
              "switch back to the compose editor"
              False
              "test subject"
              assertSubstrInOutput]

data ApplicationStep = ApplicationStep
    { asKeys :: String  -- ^ the actual commands to send
    , asDescription :: String  -- ^ step definition
    , asAsLiteralKey :: Bool  -- ^ disables key name lookup and sends literal input
    , asExpected :: String  -- ^ wait until the terminal shows the expected string or timeout
    , asAssertInOutput :: String -> String -> Assertion  -- ^ assert this against the snapshot
    }

assertSubstrInOutput :: String -> String -> Assertion
assertSubstrInOutput out substr = assertBool "in out" $ substr `isInfixOf` out

defaultSessionName :: String
defaultSessionName = "purebredtest"

tearDown :: (String, String) -> IO ()
tearDown (testdir, _)= do
  removeDirectoryRecursive testdir
  cleanUpTmuxSession defaultSessionName

setUp :: IO (String, String)
setUp = do
  (testdir, testmdir) <- setUpTmuxSession defaultSessionName >> setUpTempMaildir
  startApplication defaultSessionName testmdir
  pure (testdir, testmdir)

setUpTempMaildir :: IO (String, String)
setUpTempMaildir = do
  systmp <- getCanonicalTemporaryDirectory
  testdir <- createTempDirectory systmp defaultSessionName
  mdir <- setUpMaildir testdir
  setUpNotmuchCfg testdir mdir >>= setUpNotmuch >> pure (testdir, mdir)

-- | run notmuch to create the notmuch database
-- Note: discard stdout which otherwise clobbers the test output
setUpNotmuch :: FilePath -> IO ()
setUpNotmuch notmuchcfg = void $ readProcess "notmuch" ["--config=" <> notmuchcfg, "new"] []

-- | write a notmuch config
-- Note: currently writes a minimal config pointing to our database
setUpNotmuchCfg :: FilePath -> FilePath -> IO (FilePath)
setUpNotmuchCfg testdir testmdir = do
  let (Right ini) = parseIni (T.pack "[database]\npath=" <> T.pack testmdir)
  let nmcfg = testdir <> "/notmuch-config"
  writeIniFileWith (WriteIniSettings EqualsKeySeparator) nmcfg ini
  pure nmcfg

-- | setup a temporary Maildir for notmuch and the test session
setUpMaildir :: FilePath -> IO (FilePath)
setUpMaildir testdir = do
  let testmdir = testdir <> "/Maildir/"
  c <- getCurrentDirectory
  let maildir = c <> "/test/data/Maildir/"
  callProcess "cp" ["-r", maildir, testmdir]
  pure testmdir

-- | create a tmux session running in the background
-- Note: the width and height are the default values tmux uses, but I thought
-- it's better to be explicit.
setUpTmuxSession :: String -> IO ()
setUpTmuxSession sessionname = do
    callProcess
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
        , "purebred"]

-- | Kills the whole session including pane and application
cleanUpTmuxSession :: String -> IO ()
cleanUpTmuxSession sessionname = do
    catch
        (callProcess "tmux" ["kill-session", "-t", sessionname])
        (\e ->
              do let err = show (e :: IOException)
                 hPutStr stderr ("Exception when killing session: " ++ err)
                 pure ())


-- | Run all application steps in a session defined by session name.
tmuxSession :: String -> [ApplicationStep] -> TestTree
tmuxSession tcname xs =
  withResource setUp tearDown $
  \_ -> testCaseSteps tcname $ \step -> runSteps step xs

runSteps :: (String -> IO ()) -> [ApplicationStep] -> IO ()
runSteps stepfx steps =
    mapM_
        (\a ->
              do stepfx (asDescription a)
                 out <- performStep "purebredtest" a
                 d <- lookupEnv "DEBUG"
                 when (isJust d) $ hPutStr stderr ("\n\n" ++ asDescription a ++ "\n\n" ++ out)
                 ((asAssertInOutput a) out (asExpected a)))
        steps

performStep :: String -> ApplicationStep -> IO (String)
performStep sessionname (ApplicationStep keys _ asLiteral expect _) = do
    callProcess "tmux" $ communicateSessionArgs keys asLiteral
    baton <- newEmptyMVar
    out <- waitForString baton sessionname expect
    _ <- takeMVar baton
    pure out

holdOffTime :: Int
holdOffTime = 10^6

-- | wait for the application to render a new interface which we determine with
--   a given substring. If the expected substring is not in the captured pane,
--   wait a bit and try again.
waitForString :: MVar String -> String -> String -> IO (String)
waitForString baton sessionname substr = do
    out <- readProcess "tmux" ["capture-pane", "-e", "-p", "-t", sessionname] []
    if substr `isInfixOf` out
        then putMVar baton "ready" >> pure out
        else do
            threadDelay holdOffTime
            waitForString baton sessionname substr

-- | start the application
-- Note: this is currently defined as an additional test step for no good
-- reason.
startApplication :: String -> String -> IO ()
startApplication sessionname testmdir =
    void $
    performStep
        sessionname
        (ApplicationStep
             ("purebred --database " <> testmdir <> "\r")
             "start application"
             False
             "Purebred: Item"
             assertSubstrInOutput)

communicateSessionArgs :: String -> Bool -> [String]
communicateSessionArgs keys asLiteral =
    let base = words $ "send-keys -t " ++ defaultSessionName
        postfix =
            if asLiteral
                then ["-l"]
                else []
    in base ++ postfix ++ [keys]
