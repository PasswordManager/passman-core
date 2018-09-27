-- Copyright (C) 2017  Matthew Harm Bekkema
--
-- This file is part of passman-core.
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

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

import           Numeric.Natural (Natural)

import           Control.Concurrent.Async (Concurrently (Concurrently),
                                           runConcurrently)

import           Data.Semigroup        ((<>))
import           Data.Functor.Compose  (Compose (Compose), getCompose)
import           Data.Functor.Identity (Identity (Identity))
import           Data.Foldable         (for_)
import           Data.Traversable      (for)
import           Data.Maybe            (fromJust, isJust)
import qualified Data.Text             as T

import           Conduit ((.|), runConduitRes, runResourceT, runCatchC,
                          sourceToList, yieldMany)

import           System.IO.Temp  (withSystemTempDirectory)
import           System.FilePath ((</>))
import           System.Exit     (exitFailure)

import           Test.QuickCheck           (Arbitrary (arbitrary, shrink), Gen,
                                            Result, Property, suchThat,
                                            arbitraryBoundedEnum)
import           Test.QuickCheck.Monadic   (PropertyM, assert, run, monadicIO,
                                            pre)
import           Test.QuickCheck.Test      (isSuccess, output)
import qualified Test.QuickCheck.Unicode   as U
import           Test.QuickCheck.Instances ()

import           Passman.Core.Mode  (SingletonMode, Mode (Mode), validModes,
                                     readMode, characterCode)
import           Passman.Core.Info  (Info (Info), fromInfo)
import           Passman.Core.Entry (Entry (Entry), entryToText, textToEntry,
                                     save, append, load)
import           Passman.Core.Hash  (MasterPassword, masterPassword,
                                     fromMasterPassword, checkMasterPassword,
                                     hashMasterPassword)

import           Passman.Core.Internal.Util (fromBase, toBase)

import           PropertiesTH (ttt)


instance Arbitrary SingletonMode where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Mode where
    arbitrary = Mode <$> arbitrary

instance Arbitrary Info where
    arbitrary = Info . T.pack <$> U.string
    shrink = map (Info . T.pack) . shrink . T.unpack . fromInfo

instance Arbitrary Entry where
    arbitrary = Entry <$> arbitrary <*> arbitrary <*> arbitrary

instance Show MasterPassword where
    showsPrec i = showsPrec i . fromMasterPassword

instance Arbitrary MasterPassword where
    arbitrary = suchThatMap masterPassword (T.pack <$> U.string)

main :: IO ()
main = runTests [ $(ttt 'prop_hashMasterPassword 10)
                , $(ttt 'prop_modesValid 100)
                , $(ttt 'prop_showReadMode 100)
                , $(ttt 'prop_semigroupMode 100)
                , $(ttt 'prop_idempotentMode 100)
                , $(ttt 'prop_showReadEntry 100)
                , $(ttt 'prop_saveAppendLoadEntry 100)
                , $(ttt 'prop_toFromBase 100)
                , $(ttt 'prop_toBase10 100)
                ]

prop_hashMasterPassword :: MasterPassword -> MasterPassword -> Property
prop_hashMasterPassword x y = monadicIO $ do
    pre (x /= y)
    hashX <- run $ hashMasterPassword x
    hashY <- run $ hashMasterPassword y
    let info = "x = " ++ show x ++ ", y = " ++ show y ++ ", hashX = " ++ show hashX ++ ", hashY = " ++ show hashY ++ "."
    assertM ("(checkMasterPassword hashX x) failed, where: " ++ info) $ checkMasterPassword hashX x
    assertM ("(checkMasterPassword hashY y) failed, where: " ++ info) $ checkMasterPassword hashY y
    assertM ("(not $ checkMasterPassword hashX y) failed, where: " ++ info) $ not $ checkMasterPassword hashX y
    assertM ("(not $ checkMasterPassword hashY x) failed, where: " ++ info) $ not $ checkMasterPassword hashY x

prop_modesValid :: Mode -> Bool
prop_modesValid m = m `elem` validModes

prop_showReadMode :: Bool
prop_showReadMode = and $ do
    x <- validModes
    return (readMode (characterCode x) == Just x)

prop_semigroupMode :: Bool
prop_semigroupMode = and $ do
    x <- validModes
    y <- validModes
    z <- validModes
    return ((x <> y) <> z == x <> (y <> z))

prop_idempotentMode :: Bool
prop_idempotentMode = and $ do
    x <- validModes
    return (x == (x <> x))

prop_showReadEntry :: [Entry] -> Bool
prop_showReadEntry xs = xs == xs'
  where
    Identity xs' = sourceToList (yieldMany xs .| entryToText .| textToEntry')
    textToEntry' = runCatchC textToEntry >>= either (fail . show) pure

prop_saveAppendLoadEntry :: [Entry] -> [Entry] -> Property
prop_saveAppendLoadEntry xs ys = monadicIO $ do
    zs <- run $ withSystemTempDirectory "passman-testcase" $ \dir -> do
        let fp = dir </> "list.txt"
        runConduitRes $ yieldMany xs .| save fp
        runConduitRes $ yieldMany ys .| append fp
        runResourceT  $ sourceToList $ load fp
    assert $ (xs ++ ys) == zs

prop_toFromBase :: Natural -> Natural -> Bool
prop_toFromBase b k = fromBase (b + 2) (toBase (b + 2) k) == k

prop_toBase10 :: Natural -> Bool
prop_toBase10 0 = null $ toBase 10 0
prop_toBase10 k = toBase 10 k == map (read . pure) (show k)

assertM :: Monad m => String -> Bool -> PropertyM m ()
assertM _ True = return ()
assertM x False = fail x

suchThatMap :: (a -> Maybe b) -> Gen a -> Gen b
suchThatMap f gen = fromJust <$> fmap f gen `suchThat` isJust

runTests :: Traversable f => f (String, IO Result) -> IO ()
runTests tests = do
    results <- runConcurrently $ for tests $ \(name, prop) -> Concurrently $
        (name,) <$> prop

    allResult <- getCompose $ for_ results $ \(name, result) -> Compose $ do
        putStr $ name
        putStr $ ": "
        putStr $ output result
        pure $ if isSuccess result then Just ()
                                   else Nothing

    case allResult of
        Nothing -> exitFailure
        Just () -> pure ()
