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
-- Module      : Passman.Core.Hash
-- Copyright   : Matthew Harm Bekkema 2017
-- License     : GPL-3
-- Maintainer  : mbekkema97@gmail.com
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Passman.Core.Hash
    ( -- * Password Generation
      generatePassword
      -- * Master Password
    , MasterPassword
    , masterPassword
    , fromMasterPassword
      -- ** Hashing
    , hashMasterPassword
    , checkMasterPassword
    ) where

import           Numeric.Natural (Natural)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)

import           Data.IntCast (intCastMaybe)

import qualified Crypto.BCrypt           as BCrypt
import qualified Crypto.Hash.MD5         as MD5
import qualified Data.ByteArray.Encoding as BAE

import           Passman.Core.Info          (fromInfo)
import           Passman.Core.Entry         (Entry (Entry))
import           Passman.Core.Mode          (toCharset)
import           Passman.Core.Internal.Util (toBase, bytesToInt)


-- | Represents valid master passwords.
newtype MasterPassword = MP ByteString
  deriving Eq

-- | Turn a `MasterPassword` back into `Text`.
fromMasterPassword :: MasterPassword -> Text
fromMasterPassword (MP bs) = decodeUtf8 bs

-- | Turn `Text` into a `MasterPassword`. `Nothing` if it contains invalid bytes
-- or is too long.
masterPassword :: Text -> Maybe MasterPassword
masterPassword s = let bs = encodeUtf8 s in
  if (BS.length bs > 72) || BS.elem 0 bs
    then Nothing
    else Just $ MP bs

shorten :: Natural -> Text -> Text
shorten 0 = id
shorten n = case intCastMaybe n of
    Nothing -> shorten 0
    Just n' -> T.take n'

-- | Deterministically generate a password.
generatePassword :: Entry
                 -> MasterPassword -- ^ The master password
                 -> Text           -- ^ The generated password
generatePassword (Entry i l m) (MP p) = shorten l $ customDigest (toCharset m) $
    runBCrypt p $ MD5.hash $ encodeUtf8 $ fromInfo i

runBCrypt :: ByteString -- ^ The password to hash
          -> ByteString -- ^ A 16-byte salt
          -> ByteString -- ^ The raw BCrypt output (not base64)
runBCrypt passwd salt = case BCrypt.genSalt "$2y$" 12 salt of
    Nothing -> error "BCrypt: Invalid salt (needed 16-byte)"
    Just salt' -> case BCrypt.hashPassword passwd salt' of
        Nothing -> error "BCrypt: Unable to hash password"
        Just hash -> either error id $
            BAE.convertFromBase BAE.Base64OpenBSD $ BS.drop 29 hash

-- | Generate a hash of the `MasterPassword` to be stored in the config.
hashMasterPassword :: MasterPassword -- ^ Master password
                   -> IO Text        -- ^ Hash
hashMasterPassword (MP p) = do
    Just salt <- BCrypt.genSaltUsingPolicy BCrypt.slowerBcryptHashingPolicy
        {BCrypt.preferredHashCost = 12}
    let Just hash = BCrypt.hashPassword p salt
    return $ decodeUtf8 hash

-- | Test if the master password is correct against a hash produced by
-- `hashMasterPassword`.
--
-- >>> hash <- hashMasterPassword pass
-- >>> checkMasterPassword hash pass
-- True
checkMasterPassword :: Text           -- ^ Hash
                    -> MasterPassword -- ^ Master password
                    -> Bool
checkMasterPassword hash (MP p) = BCrypt.validatePassword (encodeUtf8 hash) p

customDigest :: Text -> ByteString -> Text
customDigest charSet cs = T.pack (T.index charSet <$> is)
  where
    is :: [Int]
    is = map fromIntegral $ toBase l (bytesToInt cs)
    -- fromIntegral: @toBase l@ returns < @l@ and @l@ fits in `Int`

    l :: Natural
    l = fromIntegral $ (T.length charSet :: Int)
    -- fromIntegral: @T.length@ returns >= 0
