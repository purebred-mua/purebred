{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
module TestUserAcceptance where

import qualified Data.Text as T
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Data.Ini (parseIni, writeIniFileWith, KeySeparator(..), WriteIniSettings(..))
import Data.Semigroup ((<>))
import Control.Concurrent (threadDelay)
import Control.Exception (catch, IOException)
import System.IO (hPutStr, stderr)
import System.Environment (lookupEnv)
import Control.Monad (void, when)
import Data.Maybe (isJust)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT, ask, ReaderT)

import Control.Lens (view, _3, _2)
import Data.List (isInfixOf)
import System.Process (callProcess, readProcess)
import System.Directory
       (getCurrentDirectory, removeDirectoryRecursive)
import Test.Tasty (TestTree, TestName, testGroup, withResource)
import Test.Tasty.HUnit (testCaseSteps, assertBool)
import Text.Regex.Posix ((=~))

systemTests ::
  TestTree
systemTests =
    testGroup
        "user acceptance tests"
        [ testUserViewsMailSuccessfully
        , testUserCanManipulateNMQuery
        , testUserCanSwitchBackToIndex
        , testCanToggleHeaders
        , testSetsMailToRead]

testSetsMailToRead ::
  TestTree
testSetsMailToRead = withTmuxSession "user can toggle read tag" $
  \step -> do
    startApplication
    liftIO $ step "mail is shown as unread (bold)"
    out <- capture
    assertRegex "\ESC\\[1;.*Testmail" out

    liftIO $ step "view mail and purebred sets it to read"
    _ <- sendKeys "Enter" "This is a test mail"
    out <- sendKeys "Escape" "is Purebred"
    assertRegex "\ESC\\[37.*Testmail" out

    liftIO $ step "toggle it back to unread"
    -- wait for the screen turns bold
    out <- sendKeys "t" "1;37;43m"
    assertRegex "\ESC\\[1;.*Testmail" out

testCanToggleHeaders ::
  TestTree
testCanToggleHeaders = withTmuxSession "user can toggle Headers" $
  \step -> do
    startApplication
    liftIO $ step "view mail"
    out <- sendKeys "Enter" "This is a test mail"
    assertSubstrInOutput "This is a test mail" out

    liftIO $ step "toggle to show all headers"
    out <- sendKeys "h" "return-path"
    assertSubstrInOutput "return-path" out

    liftIO $ step "toggle filtered headers"
    out <- sendKeys "h" "This is a test mail"
    assertRegex "Purebred.*\n.*from" out

testUserViewsMailSuccessfully ::
  TestTree
testUserViewsMailSuccessfully = withTmuxSession "user can view mail" $
  \step -> do
    startApplication
    liftIO $ step "shows tag"
    out <- capture
    assertSubstrInOutput "inbox" out

    liftIO $ step "view mail"
    out <- sendKeys "Enter" "This is a test mail"
    assertSubstrInOutput "This is a test mail" out

testUserCanManipulateNMQuery ::
  TestTree
testUserCanManipulateNMQuery =
    withTmuxSession
        "manipulating notmuch search query results in empty index" $
        \step -> do
          startApplication
          liftIO $ step "focus command"
          out <- sendKeys ":" "37;40mtag"
          assertSubstrInOutput "37;40mtag" out

          liftIO $ step "delete all input"
          out <- sendKeys "C-u" "37;40m"
          assertSubstrInOutput "37;40m" out

          liftIO $ step "enter new tag"
          _ <- sendLiteralKeys "tag:replied"

          liftIO $ step "apply"
          out <- sendKeys "Enter" "Item 0 of 1"
          assertSubstrInOutput "Item 0 of 1" out

          liftIO $ step "view currently selected mail"
          out <- sendKeys "Enter" "HOLY PUREBRED"
          assertSubstrInOutput "HOLY PUREBRED" out

testUserCanSwitchBackToIndex ::
  TestTree
testUserCanSwitchBackToIndex =
  withTmuxSession "user can switch back to mail index during composition" $
        \step -> do
            startApplication
            liftIO $ step "start composition"
            out <- sendKeys "m" "From"
            assertSubstrInOutput "From" out

            liftIO $ step "enter from email"
            out <- sendKeys "testuser@foo.test\r" "To"
            assertSubstrInOutput "To" out

            liftIO $ step "enter to: email"
            out <- sendKeys  "user@to.test\r" "Subject"
            assertSubstrInOutput "Subject" out

            liftIO $ step "enter subject"
            out <- sendKeys  "test subject\r" "~"
            assertSubstrInOutput "~" out

            liftIO $ step "enter mail body"
            out <- sendKeys  "iThis is a test body" "body"
            assertSubstrInOutput "body" out

            liftIO $ step "exit insert mode in vim"
            out <- sendKeys  "Escape" "body"
            assertSubstrInOutput "body" out

            liftIO $ step "exit vim"
            out <- sendKeys  ": x\r" "Attachments"
            assertSubstrInOutput "Attachments" out

            liftIO $ step "switch back to index"
            out <- sendKeys  "Tab" "Testmail"
            assertSubstrInOutput "Testmail" out

            liftIO $ step "switch back to the compose editor"
            out <- sendKeys  "Tab" "test subject"
            assertSubstrInOutput "test subject" out

type Env = (String, String, String)

assertSubstrInOutput :: String -> String -> ReaderT Env IO ()
assertSubstrInOutput substr out = liftIO $ assertBool (substr <> " not found in\n\n" <> out) $ substr `isInfixOf` out

assertRegex :: String -> String -> ReaderT Env IO ()
assertRegex regex out = liftIO $ assertBool (regex <> " does not match out\n\n" <> out) $ out =~ (regex :: String)

defaultSessionName :: String
defaultSessionName = "purebredtest"

tearDown :: (String, String, String) -> IO ()
tearDown (testdir, _, _)= do
  removeDirectoryRecursive testdir
  cleanUpTmuxSession defaultSessionName

setUp :: IO (String, String, String)
setUp = do
  let sessionname = defaultSessionName
  (testdir, testmdir) <- setUpTmuxSession sessionname >> setUpTempMaildir
  pure (testdir, testmdir, sessionname)

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
                 hPutStr stderr ("Exception when killing session: " <> err)
                 pure ())


-- | Run all application steps in a session defined by session name.
withTmuxSession :: TestName -> ((String -> IO ()) -> ReaderT Env IO ()) -> TestTree
withTmuxSession tcname testfx =
    withResource setUp tearDown $
      \env -> testCaseSteps tcname $ \stepfx -> env >>= runReaderT (testfx stepfx)

sendKeys :: String -> String -> ReaderT Env IO (String)
sendKeys keys expect = do
    liftIO $ callProcess "tmux" $ communicateSessionArgs keys False
    waitForString expect defaultCountdown

sendLiteralKeys :: String -> ReaderT Env IO (String)
sendLiteralKeys keys = do
    liftIO $ callProcess "tmux" $ communicateSessionArgs keys True
    waitForString keys defaultCountdown

capture :: ReaderT Env IO (String)
capture = do
  sessionname <- getSessionName
  liftIO $ readProcess "tmux" ["capture-pane", "-e", "-p", "-t", sessionname] []

getSessionName :: ReaderT Env IO (String)
getSessionName = view (_3 . ask)

getTestMaildir :: ReaderT Env IO (String)
getTestMaildir = view (_2 . ask)

holdOffTime :: Int
holdOffTime = 10^6

-- | convenience function to print captured output to STDERR
debugOutput :: String -> IO ()
debugOutput out = do
  d <- lookupEnv "DEBUG"
  when (isJust d) $ hPutStr stderr ("\n\n" <> out)

-- | wait for the application to render a new interface which we determine with
--   a given substring. If we exceed the number of tries return with the last
--   captured output, but indicate an error by setting the baton to 0
waitForString :: String -> Int -> ReaderT Env IO (String)
waitForString substr n = do
  out <- capture >>= checkPane
  liftIO $ assertBool ("Wait time exceeded. Expected: '"
                       <> substr
                       <> "' last screen shot:\n\n "
                       <> out) (substr `isInfixOf` out)
  pure out
  where
    checkPane :: String -> ReaderT Env IO String
    checkPane out
      | substr `isInfixOf` out = pure out
      | n <= 0 = pure out
      | otherwise = do
          liftIO $ threadDelay holdOffTime
          waitForString substr (n - 1)

defaultCountdown :: Int
defaultCountdown = 5

-- | start the application
-- Note: this is currently defined as an additional test step for no good
-- reason.
startApplication :: ReaderT Env IO ()
startApplication = do
  testmdir <- getTestMaildir
  liftIO $ callProcess "tmux" $ communicateSessionArgs ("purebred --database " <> testmdir <> "\r") False
  void $ waitForString "Purebred: Item" defaultCountdown

communicateSessionArgs :: String -> Bool -> [String]
communicateSessionArgs keys asLiteral =
    let base = words $ "send-keys -t " <> defaultSessionName
        postfix =
            if asLiteral
                then ["-l"]
                else []
    in base <> postfix <> [keys]
