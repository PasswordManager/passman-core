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
-- Module      : Passman.Core.Config
-- Copyright   : Matthew Harm Bekkema 2017
-- License     : GPL-3
-- Maintainer  : mbekkema97@gmail.com
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Passman.Core.Config
    ( -- * Data Structures
      Config(..)
      -- * File IO
    , loadConfig
    , saveConfig
    ) where

import Control.Exception (throwIO)

import Data.Text     (Text)
import Data.Yaml     (encodeFile, decodeFileEither)
import Data.Aeson.TH (deriveJSON, defaultOptions)

import System.FilePath          ((</>))
import System.Directory         (XdgDirectory(XdgConfig), getXdgDirectory, createDirectoryIfMissing)
import System.PosixCompat.Files (fileMode, getFileStatus, setFileMode, intersectFileModes, ownerModes)


-- | The persistent configuration of the password manager.
--
-- Intended to be saved to file in YAML format. This example config file:
--
-- >masterPasswordHash: "$2y$10$N9qo8uLOickgx2ZMRZoMyeIjZAgcfl7p92ldGxad68LJZdL17lhWy"
-- >passlistPath: "/path/to/passlist.txt"
--
-- Would be parsed as:
--
-- >Config { masterPasswordHash = "$2y$10$N9qo8uLOickgx2ZMRZoMyeIjZAgcfl7p92ldGxad68LJZdL17lhWy"
-- >       , passlistPath = "/path/to/passlist.txt"
-- >       }
data Config = Config {
                       -- | Hash of the master password generated by
                       -- `Passman.Core.Hash.hashMasterPassword`
                       masterPasswordHash :: Text
                       -- | Path to the passlist file
                     , passlistPath       :: FilePath
                     } deriving (Show, Eq)

$(deriveJSON defaultOptions ''Config)

-- | Load a `Config` from file.
loadConfig :: IO Config
loadConfig = configFile >>= decodeFileEither >>= either throwIO pure

-- | Save the specified `Config` to file, overwriting an existing config file.
saveConfig :: Config -> IO ()
saveConfig config = configFile >>= flip encodeFile config

-- | Create the path leading up to the config file with the correct permissions
-- and return the config filepath.
configFile :: IO FilePath
configFile = do
    fp <- getXdgDirectory XdgConfig "passman"
    createDirectoryIfMissing False fp
    correctPermissions fp
    pure $ fp </> "config.yaml"

-- | Remove all but the owner's permissions on the specified path.
correctPermissions :: FilePath -> IO ()
correctPermissions fp = do
    currentMode <- fileMode <$> getFileStatus fp
    let newMode = intersectFileModes currentMode ownerModes
    setFileMode fp newMode
