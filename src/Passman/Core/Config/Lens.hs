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
-- Module      : Passman.Core.Config.Lens
-- Copyright   : Matthew Harm Bekkema 2017
-- License     : GPL-3
-- Maintainer  : mbekkema97@gmail.com
-----------------------------------------------------------------------------

module Passman.Core.Config.Lens
    ( -- * Data Structures
      C.Config(Config)
    , masterPasswordHash
    , passlistPath
      -- * File IO
    , C.loadConfig
    , C.saveConfig
    ) where

import           Data.Text           (Text)
import qualified Passman.Core.Config as C


-- | Hash of the master password generated by
-- `Passman.Core.Hash.hashMasterPassword`
masterPasswordHash :: Functor f => (Text -> f Text)
                                -> C.Config -> f C.Config
masterPasswordHash f (C.Config a b) = flip C.Config b <$> f a

-- | Path to the passlist file
passlistPath :: Functor f => (FilePath -> f FilePath)
                          -> C.Config -> f C.Config
passlistPath f (C.Config a b) = C.Config a <$> f b
