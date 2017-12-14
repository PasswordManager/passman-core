-- Copyright (C) 2017  Matthew Harm Bekkema
--
-- This file is part of passman-core
--
-- passman-core is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- passman-core is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

-----------------------------------------------------------------------------
-- |
-- Module      : Passman.Core.Internal.Util
-- Copyright   : Matthew Harm Bekkema 2017
-- License     : GPL-3
-- Maintainer  : mbekkema97@gmail.com
-----------------------------------------------------------------------------

module Passman.Core.Internal.Util
    ( bytesToInt
    , fromBase
    , toBase
    ) where

import Numeric.Natural (Natural)

import Data.ByteString (ByteString, unpack)
import Data.List       (foldl', unfoldr)
import Data.Tuple      (swap)
import Data.IntCast    (intCast)


bytesToInt :: ByteString -> Natural
bytesToInt = fromBase 256 . map intCast . unpack

fromBase :: Natural -> [Natural] -> Natural
fromBase 0    = error "Base 0"
fromBase 1    = error "Base 1"
fromBase base = foldl' (\acc x -> acc * base + x) 0

toBase :: Natural -> Natural -> [Natural]
toBase base = reverse . toBaseRev base

toBaseRev :: Natural -> Natural -> [Natural]
toBaseRev 0    = error "Base 0"
toBaseRev 1    = error "Base 1"
toBaseRev base = unfoldr helper
  where
    helper :: Natural -> Maybe (Natural, Natural)
    helper 0 = Nothing
    helper i = Just $ swap $ quotRem i base
