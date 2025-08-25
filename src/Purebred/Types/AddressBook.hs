-- This file is part of purebred
-- Copyright (C) 2022 Róman Joost
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

{- |

Types for handling the retrieval of addresses

-}
module Purebred.Types.AddressBook
  (
    AddressBook(AddressBook)
  , addressBookSearch
  , addressBookRefresh
  ) where

import Control.Lens (Lens', lens)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Control.DeepSeq (NFData(rnf))

import Data.IMF (Address)

data AddressBook = AddressBook
  { _addressBookSearch :: T.Text -> IO [Address]
  , _addressBookRefresh :: Maybe (IO AddressBook)
  }
  deriving (Generic)

instance NFData AddressBook where
  rnf (AddressBook refresh search) = AddressBook refresh search `seq` ()

addressBookSearch :: Lens' AddressBook (T.Text -> IO [Address])
addressBookSearch = lens _addressBookSearch (\r x -> r { _addressBookSearch = x })

addressBookRefresh :: Lens' AddressBook (Maybe (IO AddressBook))
addressBookRefresh = lens _addressBookRefresh (\r x -> r { _addressBookRefresh = x })
