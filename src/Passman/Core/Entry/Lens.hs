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
-- Module      : Passman.Core.Entry.Lens
-- Copyright   : Matthew Harm Bekkema 2017
-- License     : GPL-3
-- Maintainer  : mbekkema97@gmail.com
-----------------------------------------------------------------------------

module Passman.Core.Entry.Lens
    ( -- * Data structures
      E.Entry (Entry)
    , info
    , maxLength
    , mode
      -- * Parsing
    , E.load
    , E.textToEntry
    , E.csvToEntry
      -- * Saving
    , E.save
    , E.append
    , E.entryToText
    , E.entryToCsv
    ) where

import           Numeric.Natural    (Natural)
import           Passman.Core.Info  (Info)
import           Passman.Core.Mode  (Mode)
import qualified Passman.Core.Entry as E


-- | The info string to generate the password for.
info :: Functor f => (Info -> f Info) -> E.Entry -> f E.Entry
info f (E.Entry a b c) = flip (flip E.Entry b) c <$> f a

-- | Maximum length of the generated password or 0 if no maximum length.
maxLength :: Functor f => (Natural -> f Natural) -> E.Entry -> f E.Entry
maxLength f (E.Entry a b c) = flip (E.Entry a) c <$> f b

-- | The mode to use when generating the password.
mode :: Functor f => (Mode -> f Mode) -> E.Entry -> f E.Entry
mode f (E.Entry a b c) = E.Entry a b <$> f c
