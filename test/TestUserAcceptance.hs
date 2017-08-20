{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
module TestUserAcceptance where

import qualified Data.Text as T
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Data.Ini (parseIni, writeIniFileWith, KeySeparator(..), WriteIniSettings(..))
import Data.Semigroup ((<>))
import Control.Concurrent
       (newEmptyMVar, putMVar, takeMVar)
import System.Timeout (timeout)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
       (register, release, resourceForkIO, runResourceT, ResourceT, ReleaseKey)
import Control.Exception (catch, IOException)
import System.IO (hPutStr, stderr)
import Control.Monad (void)

import System.Process (callProcess, readProcess)
import System.Directory
       (getCurrentDirectory, removeFile, getTemporaryDirectory,
        removeDirectoryRecursive)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

import Network.Socket hiding (recv)
import Network.Socket
       (bind, socket, Family(..), SocketType(..), defaultProtocol, SockAddr(..))
import Network.Socket.ByteString (recv)
import Data.ByteString.Char8 (pack, ByteString)
import qualified Data.ByteString.Lazy as LBS

systemTests ::
  TestTree
systemTests =
    testGroup
        "result in the right state"
        [testUserViewsMailSuccessfully, testUserCanSwitchBackToIndex]

testUserViewsMailSuccessfully ::
  TestTree
testUserViewsMailSuccessfully =
    goldenVsString
        "user can view mail"
        "test/data/viewMail.golden"
        (runResourceT $ tmuxSession steps "purebredtest")
  where
    steps = [ApplicationStep "Enter" False]

testUserCanSwitchBackToIndex ::
  TestTree
testUserCanSwitchBackToIndex =
    goldenVsString
        "user can manipulate search query"
        "test/data/manipulateNotmuchQuery.golden"
        (runResourceT $ tmuxSession steps "purebredtest")
  where
    steps =
        [ ApplicationStep ":" False
        , ApplicationStep "Down" False
        , ApplicationStep "C-u" False
        , ApplicationStep "tag:foo" True
        , ApplicationStep "Enter" False
        , ApplicationStep "Escape" False
        ]


data ApplicationStep = ApplicationStep
    { asCommand :: String  -- ^ the actual commands to send
    , asAsLiteralKey :: Bool  -- ^ disables key name lookup and sends literal input
    }

tmuxSession :: [ApplicationStep] -> String -> ResourceT IO (LBS.ByteString)
tmuxSession xs sessionname = do
    systmp <- liftIO $ getCanonicalTemporaryDirectory
    testdir <- liftIO $ createTempDirectory systmp "purebredtest"
    mdir <-
        liftIO $
        do mdir <- prepareMaildir testdir
           prepareNotmuchCfg testdir mdir >>= prepareNotmuch >> pure mdir
    tmuxRkey <- createTmuxSession sessionname
    startApplication mdir
    outputfile <-
        liftIO $
        do runSteps xs
           snapshotState sessionname testdir
    release tmuxRkey
    -- only remove the tempdir if the whole session run was without problems,
    -- otherwise it'll help to debug issues
    tout <- liftIO $ LBS.readFile outputfile
    liftIO $ removeDirectoryRecursive testdir
    pure tout

runSteps :: [ApplicationStep] -> IO ()
runSteps steps =
    mapM_
        (\(ApplicationStep xs asLiteral) ->
              callProcess "tmux" $ communicateSessionArgs xs asLiteral)
        steps

snapshotState :: String -> FilePath -> IO (FilePath)
snapshotState sessionname testdir = do
    let fp = testdir <> "/" <> sessionname <> "paneoutput.log"
    readProcess "tmux" hardcopyArgs [] >>= writeFile fp
    pure fp
    where
      hardcopyArgs = ["capture-pane", "-p", "-t", sessionname]

startApplication :: String -> ResourceT IO ()
startApplication testmdir = do
    liftIO $ do runSteps [ApplicationStep ("purebred --database " <> testmdir) True, ApplicationStep "Enter" False]
    -- prepare thread waiting for purebred to signal readiness
    baton <- liftIO $ newEmptyMVar
    sockAddr@(SockAddrUnix sfile) <- purebredSocketAddr
    rkey <- register (removeFile sfile)
    _ <- resourceForkIO $ waitReady sockAddr >> do liftIO $ putMVar baton "ready"
    -- purebred should be running by now, send the "special" key for purebred to
    -- connect to the socket and send ready
    liftIO $ do runSteps [ApplicationStep "C-t" False]
                void $ timeout applicationStartupTimeout $ takeMVar baton
    -- clean up socket file
    release rkey

createTmuxSession :: String -> ResourceT IO ReleaseKey
createTmuxSession sessionname = do
    liftIO $
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
    register (cleanUpTmuxSession sessionname)

cleanUpTmuxSession :: String -> IO ()
cleanUpTmuxSession sessionname = do
    catch
        (callProcess "tmux" ["kill-session", "-t", sessionname])
        (\e ->
              do let err = show (e :: IOException)
                 hPutStr stderr ("Exception when killing session: " ++ err)
                 pure ())

purebredSocketAddr :: ResourceT IO SockAddr
purebredSocketAddr = do
  tmp <- liftIO $ getTemporaryDirectory
  let socketfile = (tmp <> "/purebred.socket")
  pure $ SockAddrUnix socketfile

waitReady :: SockAddr -> ResourceT IO ()
waitReady addr = do
    liftIO $
        do soc <- socket AF_UNIX Datagram defaultProtocol
           bind soc addr
           d <- recv soc 4096
           if d /= applicationReadySignal
               then error "application did not start up in time"
               else close soc

applicationReadySignal :: ByteString
applicationReadySignal = pack "READY=1"

applicationStartupTimeout :: Int
applicationStartupTimeout = 10 ^ 6 * 6

communicateSessionArgs :: String -> Bool -> [String]
communicateSessionArgs keys asLiteral =
    let base = words "send-keys -t purebredtest"
        postfix =
            if asLiteral
                then ["-l"]
                else []
    in base ++ postfix ++ [keys]

-- run notmuch to create the notmuch database
-- Note: discard stdout which otherwise clobbers the test output
prepareNotmuch :: FilePath -> IO ()
prepareNotmuch notmuchcfg = void $ readProcess "notmuch" ["--config=" <> notmuchcfg, "new"] []

prepareNotmuchCfg :: FilePath -> FilePath -> IO (FilePath)
prepareNotmuchCfg testdir testmdir = do
  let (Right ini) = parseIni (T.pack "[database]\npath=" <> T.pack testmdir)
  let nmcfg = testdir <> "/notmuch-config"
  writeIniFileWith (WriteIniSettings EqualsKeySeparator) nmcfg ini
  pure nmcfg

prepareMaildir :: FilePath -> IO (FilePath)
prepareMaildir testdir = do
  let testmdir = testdir <> "/Maildir/"
  c <- getCurrentDirectory
  let maildir = c <> "/test/data/Maildir/"
  callProcess "cp" ["-r", maildir, testmdir]
  pure testmdir
