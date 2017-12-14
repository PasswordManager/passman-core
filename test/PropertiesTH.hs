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

{-# LANGUAGE TemplateHaskellQuotes #-}

module PropertiesTH
    ( ttt
    ) where

import Test.QuickCheck     (Testable, Result, Args (maxSuccess, chatty),
                            quickCheckWithResult, stdArgs)
import Language.Haskell.TH (Q, Name, Exp (AppE, VarE, LitE),
                            Lit (StringL, IntegerL))


ttt :: Name -> Int -> Q Exp
ttt name len = pure $ (VarE $ 'ttt') `AppE`
                      (LitE $ StringL $ show name) `AppE`
                      (LitE $ IntegerL $ fromIntegral len) `AppE`
                      (VarE name)

ttt' :: Testable prop => String -> Int -> prop -> (String, IO Result)
ttt' name len prop = (name, quickCheckWithResult myArgs prop)
  where
    myArgs = stdArgs { maxSuccess = len, chatty = False }
