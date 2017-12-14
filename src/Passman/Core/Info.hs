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
-- Module      : Passman.Core.Info
-- Copyright   : Matthew Harm Bekkema 2017
-- License     : GPL-3
-- Maintainer  : mbekkema97@gmail.com
-----------------------------------------------------------------------------

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Passman.Core.Info
    ( Info (Info)
    , fromInfo
    ) where

import Control.Arrow  (first)

import Data.Semigroup (Semigroup)
import Data.Coerce    (coerce)
import Data.String    (IsString (fromString))
import Data.Text      (Text, strip)


-- | Represents an info string to be used in an "Passman.Core.Entry".
newtype Info = MkInfo { -- | Turn an `Info` back into `Text`
                        fromInfo :: Text
                      }
  deriving (Eq, Ord, Semigroup, Monoid)

instance Show Info where
  showsPrec = coerce (showsPrec :: Int -> Text -> ShowS)

instance Read Info where
  readsPrec i = fmap (first Info) . readsPrec i

instance IsString Info where
  fromString = Info . fromString

pattern Info :: Text -> Info
pattern Info x <- MkInfo x
  where
    Info = MkInfo . strip

{-# COMPLETE Info #-}
