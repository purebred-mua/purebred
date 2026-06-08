-- This file is part of purebred
-- Copyright (C) 20216 Róman Joost
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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Undo System -}
module Purebred.Undo
  ( maxUndo
  , recordUndo
  , popApply
  , applyTagOps
  , applyTagOpsWithUndo
  , runReverse
  , runForward
  ) where

import qualified Brick.Widgets.List as L
import Data.Foldable (toList)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Monad.State (MonadState)
import Control.Monad.Reader (MonadIO)
import Control.Monad.Except (ExceptT(..), runExceptT, MonadError)
import Control.Lens (Lens', (&), set, each, view, use, modifying, assign, _2)

import Purebred.Types
import Purebred.Types.Error
import qualified Purebred.Storage.Client
import qualified Purebred.Storage.Tags


maxUndo :: Int
maxUndo = 50

recordUndo :: MonadState AppState m => Undoable -> m ()
recordUndo u = do
  modifying (asUndoStack . usUndo) (take maxUndo . (u :))
  assign (asUndoStack . usRedo) []

-- | undo: per-mail, because each snapshot has its own prior tags
runReverse :: (MonadIO m, MonadState AppState m) => Undoable -> m (Either Error ())
runReverse (UndoMailTags _ before) = runExceptT $ do
  traverse apply before >>= reconcileTags . concat
  where
    apply m = ExceptT (applyTagOps (Purebred.Storage.Tags.restoreOps m) [m])

runForward :: (MonadIO m, MonadState AppState m) => Undoable -> m (Either Error ())
runForward (UndoMailTags ops before) = runExceptT $
  ExceptT (applyTagOps ops before) >>= reconcileTags

reconcileTags :: (MonadError Error m, MonadIO m, MonadState AppState m) => [NotmuchMail] -> m ()
reconcileTags updated = do
  let byMailId = Map.fromList [ (view mailId m, view Purebred.Storage.Tags.tags m) | m <- updated ]
      patchMail m = maybe m (\ts -> m & set Purebred.Storage.Tags.tags ts) (Map.lookup (view mailId m) byMailId)
  modifying (asThreadsView . miListOfMails . L.listElementsL . each . _2) patchMail

  server <- use storageServer
  let tids = Set.toList . Set.fromList $ view mailThreadId <$> updated
  v <- Purebred.Storage.Client.getThreadsByIds tids server
  let byThreadId = Map.fromList [ (view thId t, t) | t <- toList v ]
      patchThread t = Map.findWithDefault t (view thId t) byThreadId
  modifying (asThreadsView . miListOfThreads . L.listElementsL . each . _2) patchThread

popApply
  :: (MonadIO m, MonadState AppState m)
  => Lens' AppState [Undoable]
  -> Lens' AppState [Undoable]
  -> (Undoable -> m (Either Error ()))
  -> T.Text
  -> m ()
popApply src dst run verb = do
  stack <- use src
  case stack of
    [] -> assign asUserMessage (Just $ UserMessage StatusBar $ Info (verb <> ": nothing to do"))
    (u : undostack) ->
        run u >>= \case
          Left err -> assign asUserMessage (Just $ UserMessage ManageMailTagsEditor $ Error err)
          Right () -> do
            assign src undostack
            modifying dst (u :)
            let remaining = length undostack
                msg = verb <> ": " <> describeUndoable u
                      <> if remaining > 0
                         then T.pack $ " (" <> show remaining <> "more)"
                         else ""
            assign asUserMessage (Just $ UserMessage StatusBar $ Info msg)

describeUndoable :: Undoable -> T.Text
describeUndoable (UndoMailTags _ mails) =
  let amount = length mails
  in T.pack $ show amount <> " message(s)"

-- | Apply given tag operations on all mails
--
applyTagOps
  :: (Traversable t, MonadIO m, MonadState AppState m)
  => [TagOp]
  -> t NotmuchMail
  -> m (Either Error (t NotmuchMail))
applyTagOps ops mails = do
  server <- use storageServer
  runExceptT (Purebred.Storage.Client.messageTagModify ops mails server)

-- | Apply given tag operations on all mails
--
applyTagOpsWithUndo
  :: (Traversable t, MonadIO m, MonadState AppState m)
  => [TagOp]
  -> t NotmuchMail
  -> m (Either Error (t NotmuchMail))
applyTagOpsWithUndo ops mails = do
  server <- use storageServer
  result <- runExceptT (Purebred.Storage.Client.messageTagModify ops mails server)
  case result of
    Right newMails -> do
      let undoop = UndoMailTags ops (toList mails)
      recordUndo undoop
      pure (Right newMails)
    Left e -> pure (Left e)
