-- This file is part of purebred
-- Copyright (C) 2018 Róman Joost
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
module Network.Mail.Mime.Lens where

import Control.Lens (Lens')
import Network.Mail.Mime (Mail(..), Alternatives, Address, Headers)


lMailParts :: Lens' Mail [Alternatives]
lMailParts f (Mail a b c d e f') = fmap (\f'' -> Mail a b c d e f'') (f f')

lMailTo :: Lens' Mail [Address]
lMailTo f (Mail a b c d e g) = fmap (\b' -> Mail a b' c d e g) (f b)

lMailFrom :: Lens' Mail Address
lMailFrom f (Mail a b c d e g) = fmap (\a' -> Mail a' b c d e g) (f a)

lMailHeaders :: Lens' Mail Headers
lMailHeaders f (Mail a b c d e g) = fmap (\e' -> Mail a b c d e' g) (f e)
