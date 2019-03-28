-- This file is part of purebred
-- Copyright (C) 2019 Róman Joost
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

module Purebred.System.Process
  ( tryRunProcess
  , handleIOException
  , handleExitCode

  -- * Re-exports from @System.Process.Typed@
  , ProcessConfig
  , proc
  , setStdin
  , byteStringInput
  ) where

import System.Exit (ExitCode(..))
import Control.Exception (try, IOException)
import System.Process.Typed
import qualified Data.ByteString.Lazy as LB
import Control.Lens (set, (&))
import Data.Semigroup ((<>))

import qualified Data.Text as T

import Error
import Types
import Purebred.Types.IFC


-- | Handler to handle exit failures and possibly showing an error in the UI.
handleExitCode :: AppState -> (ExitCode, Tainted LB.ByteString) -> AppState
handleExitCode s (ExitFailure e, stderr) =
  s & setError (ProcessError (
    show e <> ": " <> untaint (T.unpack . sanitiseText . decodeLenient . LB.toStrict) stderr))
handleExitCode s (ExitSuccess, _) = s

-- | Handle only IOExceptions, everything else is fair game.
handleIOException :: AppState -> IOException -> IO AppState
handleIOException s' ex = pure $ s' & setError (ProcessError (show ex))

-- | Try running a process given by the `FilePath` and catch an IOExceptions.
-- This is to avoid a crashing process also take down the running Brick program.
--
-- Returns the exit code and the standard error output.
--
tryRunProcess
  :: ProcessConfig stdout stderrIgnored stdin
  -> IO (Either IOException (ExitCode, Tainted LB.ByteString))
tryRunProcess = (fmap . fmap . fmap) taint . try . readProcessStderr

setError :: Error -> AppState -> AppState
setError = set asError . Just
