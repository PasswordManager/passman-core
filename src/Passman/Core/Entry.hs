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
-- Module      : Passman.Core.Entry
-- Copyright   : Matthew Harm Bekkema 2017
-- License     : GPL-3
-- Maintainer  : mbekkema97@gmail.com
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Passman.Core.Entry
    ( -- * Data structures
      Entry (..)
      -- * Parsing
    , load
    , textToEntry
    , csvToEntry
      -- * Saving
    , save
    , append
    , entryToText
    , entryToCsv
    ) where

import           Conduit (MonadResource, MonadThrow, throwM, ConduitT, (.|),
                          mapC, mapMC, sourceFile, sinkFile, sinkIOHandle,
                          decodeUtf8C, encodeUtf8C)

import           Control.Monad                (mfilter)

import           Data.CSV.Conduit    (intoCSV, fromCSV, defCSVSettings)

import           Numeric.Natural (Natural)
import           Text.Read       (readMaybe)

import           Data.Text (Text)
import qualified Data.Text as T

import           System.IO (IOMode (AppendMode), openBinaryFile)

import           Passman.Core.Mode (Mode, readMode, defaultMode, characterCode)
import           Passman.Core.Info (Info (Info), fromInfo)


-- | Represents an entry in the password list.
--
-- Password lists are intended to be saved to file in UTF8 CSV format. All
-- fields except for `info` are optional and can be left blank for a default
-- value.
--
-- This example password list file:
--
-- >google.com
-- >projecteuler.net,32
-- >wiki.haskell.org,,nl
--
-- Would be parsed as:
--
-- >[ Entry {info = "google.com", maxLength = 0, mode = Mode (ModeN :| [ModeC,ModeL])}
-- >, Entry {info = "projecteuler.net", maxLength = 32, mode = Mode (ModeN :| [ModeC,ModeL])}
-- >, Entry {info = "wiki.haskell.org", maxLength = 0, mode = Mode (ModeN :| [ModeL])}
-- >]
data Entry = Entry
    { -- | The info string to generate the password for.
      info :: Info

      -- | Maximum length of the generated password or 0 if no maximum length.
    , maxLength :: Natural

      -- | The mode to use when generating the password.
    , mode :: Mode
    }
    deriving (Show, Eq)

-- | Load `Entry`s from the specified file.
load :: (MonadThrow m, MonadResource m) => FilePath -> ConduitT i Entry m ()
load f = sourceFile f .| decodeUtf8C .| textToEntry

-- | Convert `Text` into `Entry`s.
textToEntry :: MonadThrow m => ConduitT Text Entry m ()
textToEntry = intoCSV defCSVSettings .| mapMC csvToEntry

-- | Attempt to convert CSV fields into an `Entry`.
csvToEntry :: MonadThrow m => [Text] -> m Entry
csvToEntry []      = failM "no info column"
csvToEntry [a]     = pure $ Entry (Info a) 0 defaultMode
csvToEntry [a,b]   = flip (Entry (Info a)) defaultMode <$> parseLength b
csvToEntry [a,b,c] = Entry (Info a) <$> parseLength b <*> parseMode c
csvToEntry _       = failM "too many columns"

-- | Save `Entry`s to the specified file.
save :: MonadResource m => FilePath -> ConduitT Entry o m ()
save f = entryToText .| encodeUtf8C .| sinkFile f

-- | Append `Entry`s to the specified file.
append :: MonadResource m => FilePath -> ConduitT Entry o m ()
append fp = entryToText .| encodeUtf8C .| sinkIOHandle
    (openBinaryFile fp AppendMode)

-- | Convert an `Entry` into `Text` for saving to file.
entryToText :: Monad m => ConduitT Entry Text m ()
entryToText = mapC entryToCsv .| fromCSV defCSVSettings

-- | Convert an `Entry` into CSV fields.
entryToCsv :: Entry -> [Text]
entryToCsv (Entry i l m) =
    [ fromInfo i
    , if l == 0 then T.empty else T.pack $ show l
    , T.pack $ characterCode m
    ]

parseLength :: MonadThrow m => Text -> m Natural
parseLength = parseOrError
    (parseWithOrEmpty (mfilter (>0) . readMaybe . T.unpack) 0)
    "invalid length"

parseMode :: MonadThrow m => Text -> m Mode
parseMode = parseOrError (parseWithOrEmpty (readMode . T.unpack) defaultMode)
                         "invalid mode"

parseWithOrEmpty :: (Text -> Maybe a) -> a -> Text -> Maybe a
parseWithOrEmpty p d x = if T.null x' then Just d else p x'
  where
    x' = T.strip x

parseOrError :: MonadThrow m => (a -> Maybe b) -> String -> a -> m b
parseOrError f err = maybe (failM err) pure . f

failM :: MonadThrow m => String -> m a
failM = throwM . userError
