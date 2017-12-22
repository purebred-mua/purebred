-- This file is part of purebred
-- Copyright (C) 2017 Fraser Tweedale and Róman Joost
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
module Purebred.Tags where

import Text.Parsec
       (runParser, spaces, char, sepBy, many1, spaces, letter)
import Text.Parsec.Text (Parser)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Prelude hiding (take, takeWhile)
import Types
import Control.Lens (over, _Left)
import Data.Text (Text, pack)
import Error


tagOp :: Parser TagOp
tagOp =
  (char '+' *> (AddTag <$> (pack <$> many1 letter)))
  <|> (char '-' *> (RemoveTag <$> (pack <$> many1 letter)))

resetOp :: Parser TagOp
resetOp = char '=' $> ResetTags

allTagOps :: Parser [TagOp]
allTagOps = tagOp `sepBy` spaces

tagOpsWithReset :: Parser [TagOp]
tagOpsWithReset = do
  r <- resetOp
  spaces
  ops <- allTagOps
  pure $ r : ops

parseTagOps :: Text -> Either Error [TagOp]
parseTagOps txt = let parsed = runParser (tagOpsWithReset <|> allTagOps) () "" txt
                  in over _Left (GenericError . show) parsed
