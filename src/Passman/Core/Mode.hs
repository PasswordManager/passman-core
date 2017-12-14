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
-- Module      : Passman.Core.Mode
-- Copyright   : Matthew Harm Bekkema 2017
-- License     : GPL-3
-- Maintainer  : mbekkema97@gmail.com
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Passman.Core.Mode
    ( -- * Mode
      Mode (Mode)
    , SingletonMode (..)
      -- * Constants
    , validModes
    , defaultMode
      -- * Parsing modes
    , characterCode
    , readMode
      -- * Convert mode to character set
    , toCharset
    ) where

import           Control.Monad      ((<=<))

import           Data.Coerce        (coerce)
import           Data.Semigroup     (Semigroup ((<>), stimes), stimesIdempotent)
import           Data.String        (IsString, fromString)
import           Data.List          (sort, subsequences)
import           Data.Maybe         (mapMaybe)

import           Data.List.NonEmpty (NonEmpty((:|)), nonEmpty, toList)
import qualified Data.List.NonEmpty as NE

import qualified Data.List.Ordered  as O


-- | Represents the sets of characters that generated passwords may contain.
-- Inspired by the mode parameter to [apg](https://linux.die.net/man/1/apg).
newtype Mode = MkMode { fromMode :: NonEmpty SingletonMode }
    deriving Eq

data SingletonMode = ModeS -- ^ Represents special characters, that is:
                           -- !\"#$%&\'()*+,-./:;\<\=\>?\@[\\]^_\`{|}~
                   | ModeN -- ^ Represents numbers
                   | ModeC -- ^ Represents capital letters
                   | ModeL -- ^ Represents lowercase letters
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Show Mode where
    showsPrec = coerce (showsPrec :: Int -> NonEmpty SingletonMode -> ShowS)

instance Semigroup Mode where
    (Mode x) <> (Mode y) = MkMode $ NE.fromList $ O.union (toList x) (toList y)
    stimes = stimesIdempotent

pattern Mode :: NonEmpty SingletonMode -> Mode
pattern Mode x <- MkMode x
  where
    Mode = MkMode . NE.fromList . O.nubSort . toList

{-# COMPLETE Mode #-}

-- | List of every valid `Mode`.
validModes :: [Mode]
validModes = mapMaybe (fmap Mode . nonEmpty) $
    subsequences [ModeS, ModeN, ModeC, ModeL]

-- | The combination of `ModeN`, `ModeC` and `ModeL`
defaultMode :: Mode
defaultMode = Mode $ ModeN :| [ModeC, ModeL]

-- | Turn a mode back into a character code that can be read by `readMode`.
characterCode :: IsString a => Mode -> a
characterCode = fromString . map helper . toList . fromMode
  where
    helper ModeS = 's'
    helper ModeN = 'n'
    helper ModeC = 'c'
    helper ModeL = 'l'

-- | Reads a string for the characters: 's', 'n', 'c' or 'l'. Constructs a
-- `Mode` based on those characters where 's' represents `ModeS`, 'n' represents
-- `ModeN`, 'c' represents `ModeC` and 'l' represents `ModeL`. Returns `Nothing`
-- if the string contains invalid characters. Returns `defaultMode` for the
-- empty string.
readMode :: String -> Maybe Mode
readMode = fmap Mode . nonEmpty <=< traverse helper
  where
    helper :: Char -> Maybe SingletonMode
    helper 's' = Just ModeS
    helper 'n' = Just ModeN
    helper 'c' = Just ModeC
    helper 'l' = Just ModeL
    helper  _  = Nothing

-- | Get the character set that the specified `Mode` represents
toCharset :: IsString a => Mode -> a
toCharset = fromString . sort . concatMap helper . toList . fromMode
  where
    helper :: SingletonMode -> String
    helper ModeS = symbols
    helper ModeN = numbers
    helper ModeC = upper
    helper ModeL = lower

lower, upper, numbers, symbols :: String
lower = ['a'..'z']
upper = ['A'..'Z']
numbers = ['0'..'9']
symbols = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
