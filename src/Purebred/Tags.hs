-- This file is part of purebred
-- Copyright (C) 2017-2021 Fraser Tweedale and Róman Joost
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
--

-- | This module provides functions to parse tagging operations. These
-- operations are used for annotating mails or threads with
-- labels. Notmuch does not support the labelling of threads as such
-- even though Purebred exposes the functionality. Labeling threads
-- just means labelling all mails in a thread.
--
module Purebred.Tags
  ( parseTag
  , parseTagOps
  ) where

import Control.Applicative ((<|>), optional)
import qualified Data.Attoparsec.Internal.Types as AT
import Data.Attoparsec.ByteString.Char8
  ( Parser, parseOnly, isSpace, space, char, sepBy
  , skipMany1, takeWhile1, endOfInput, peekChar' )
import qualified Data.ByteString as B
import Data.Functor (($>))
import Control.Lens (over, _Left)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Notmuch (mkTag)

import Types
import UI.Notifications (makeWarning)

tagOp :: Parser TagOp
tagOp =
  (char '+' *> (AddTag <$> takeTag))
  <|> (char '-' *> (RemoveTag <$> takeTag))
  where
  takeTag = takeWhile1 (not . isSpace) >>= parseTag

resetOp :: Parser TagOp
resetOp = char '=' $> ResetTags

-- | skip whitespace.  fails on no whitespace
skipSpaces :: Parser ()
skipSpaces = skipMany1 space

allTagOps :: Parser [TagOp]
allTagOps = tagOp `sepBy` skipSpaces

tagOpsWithReset :: Parser [TagOp]
tagOpsWithReset = do
  r <- resetOp
  _ <- optional skipSpaces -- no space needed after '='
  ops <- allTagOps
  pure $ r : ops

parseTagOps :: T.Text -> Either UserMessage [TagOp]
parseTagOps = over _Left (makeWarning StatusBar . T.pack) . parseOnly p . T.encodeUtf8
  where
  p =
    (tagOpsWithReset <|> allTagOps)
    <* optional skipSpaces  -- skip any trailing whitespace
    <* niceEndOfInput       -- assert EOF

parseTag :: B.ByteString -> Parser Tag
parseTag s = maybe
  (fail $ "not a valid tag: " <> show s)
  pure
  (mkTag s)

-- | Assert end of input has been reached,
-- or fail with a message that includes the
-- problematic character and the offset.
niceEndOfInput :: Parser ()
niceEndOfInput = endOfInput <|> p
  where
  p = do
    c <- peekChar'
    off <- offset
    fail $ "unexpected " <> show c <> " at offset " <> show off

-- | Get the current position of the parser
offset :: AT.Parser i Int
offset = AT.Parser $ \t pos more _lose suc -> suc t pos more (AT.fromPos pos)
