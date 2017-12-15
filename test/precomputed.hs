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

{-# LANGUAGE OverloadedStrings #-}

import           Data.Functor.Identity (Identity (Identity))

import           Data.Conduit       ((.|), yield, sourceToList)
import           Data.Conduit.Lift  (runCatchC)

import           Data.Maybe         (fromJust)
import           Data.Yaml          (decode)
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.List.NonEmpty (NonEmpty ((:|)))

import           Passman.Core.Mode   (SingletonMode (..), Mode (Mode),
                                      defaultMode)
import           Passman.Core.Hash   (generatePassword, masterPassword)
import           Passman.Core.Entry  (Entry (Entry), textToEntry)
import           Passman.Core.Info   (Info (Info))
import           Passman.Core.Config (Config (Config))


main :: IO ()
main = do
    testPrecomputed configExamples
    testPrecomputed entryExamples
    testPrecomputed hashCompat

testPrecomputed :: (Foldable f, Eq a, Show a) => f (a, a) -> IO ()
testPrecomputed = mapM_ $ \(x, y) -> if x == y then pure () else
    fail $ "Test failed, wanted: " ++ show x ++ ", got: " ++ show y

-- | Ensure the examples in "Passman.Core.Config" are correct.
configExamples :: [(Config, Config)]
configExamples = zip' wanted got
  where
    got = map (fromJust . decode . encodeUtf8 . T.unlines)
              [ ["masterPasswordHash: \"$2y$10$N9qo8uLOickgx2ZMRZoMyeIjZAgcfl7p92ldGxad68LJZdL17lhWy\""
                ,"passlistPath: \"/path/to/passlist.txt\""]
              ]
    wanted = [ Config "$2y$10$N9qo8uLOickgx2ZMRZoMyeIjZAgcfl7p92ldGxad68LJZdL17lhWy" "/path/to/passlist.txt"
             ]

-- | Ensure the examples in "Passman.Core.Entry" are correct.
entryExamples :: [(Entry, Entry)]
entryExamples = zip' wanted got
  where
    Identity got = sourceToList $
                       runCatchC (yield input .| textToEntry) >>=
                       either (fail . show) pure
    wanted = [ Entry "google.com"        0 $ Mode $ ModeN :| [ModeC,ModeL]
             , Entry "projecteuler.net" 32 $ Mode $ ModeN :| [ModeC,ModeL]
             , Entry "wiki.haskell.org"  0 $ Mode $ ModeN :| [ModeL]
             ]
    input = T.unlines [ "google.com"
                      , "projecteuler.net,32"
                      , "wiki.haskell.org,,nl"
                      ]

-- | Ensure the same passwords are generated in the future
hashCompat = zip' wanted got
  where
    got = do
        site <- [ "google.com"
                , "twitter.com"
                , "github.com"
                , "projecteuler.net"
                ]
        mode <- map (Mode . pure) [ModeN, ModeC, ModeL, ModeS] ++ [defaultMode]
        pass <- ["password", "asdfghjkl;"]
        return $ generatePassword (Entry (Info site) 0 mode) $ fromJust $
            masterPassword pass
    wanted = ["17810818903316760060773059737533795991133745607244713632","14971578877009723178083623608509425124977824730063617147","BEIGUKMIRVADCGKPJFARIURSWINFVWGGFFZJDCOE","ZMPRZEOLYNLGSKVDMPORLIABGSZQCHNFHTMYCZJ","beigukmirvadcgkpjfariurswinfvwggffzjdcoe","zmprzeolynlgskvdmporliabgszqchnfhtmyczj",",>|\"!_-^,>{;\"@}~~/)(\".+@-)@-({)$\"'$@!","*]>{\"}{.})':;@'+/$&\".)^<={)>^~>)<<:$`","U7m8cqAIOqHoKZSy09DB33v0l1Ikjho","PK20RJ2Bps04UobqaqS20eRBF18IxER","16213550557837790768191330245979107764650558081656003841","4024181751675677039254525579814061978376990589883699557","BBPOVHUMTRJNVZUMQCPKPHCUPXKQNAOPZURCKDCR","GWCPOCUVQKSBVNEZPBCKFCIIBQRAAZUBSBQEYRL","bbpovhumtrjnvzumqcpkphcupxkqnaopzurckdcr","gwcpocuvqksbvnezpbckfciibqraazubsbqeyrl","+=<]\":%~'`&=}\\/^/||?_)}!@}@[`/'{=`()\"","#?!|;{/-=~:`;^}->*|^\"{`!=$[*%!?:|.?,&","RQH2ZyxOKG0AlgbrPzOxxwE5LUwosL3","6o0Mw8O0uwOgDPnW7N2eIFRJsDTUrUD","22987601634120402559542468265169167564948676216524901239","8353733280890735007293224669721856622313099986997974732","BNDJRMODRDBMSQVQVIDBKDVFPBPDFGCOQOXKONEL","OFSPDTEPACOLGNAELKQNKCMVNQILPNQNVNTGIGQ","bndjrmodrdbmsqvqvidbkdvfpbpdfgcoqoxkonel","ofspdtepacolgnaelkqnkcmvnqilpnqnvntgigq",":!!$>*_'/?&^^_[@>{]*#_;[/~~_!%/)))#`\\","&/.{;))}),$*^\\*&+^#,[#@*};##>@|+=>&[-","cseTPcHeM3fP3a4sXuDMqPx6uT1IKsp","E82BiPtvSYeIwF2kgxwEbt6iFDhMlrQ","15823457199654189728019009138314459330131805865815516925","23809039512397779922479254294240900352779187010885878014","BAYHYMMBPBPQXOACZSSLUXRJNGOYAFEASJYOIZFX","BONSXHELIEYAUPYXVYJWUVZCQOIGWORUQGRJJWWS","bayhymmbpbpqxoaczssluxrjngoyafeasjyoizfx","bonsxhelieyaupyxvyjwuvzcqoigworuqgrjjwws","++.$!]<#}@&\\,\"?\\}=[*^*&^:`+:~,|';/*\\|",":<%~@|%{,'_%-*%?|*#}\\\"#[\"*)/>$${\\(!(}","QlMjkg1KjUzNRpg1WkDKSndPnzyMrKX","eGnHr1DIalnw12J08mvJThB4afy1iyk"]

zip' :: [a] -> [b] -> [(a, b)]
zip' [] [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
zip' _ _ = error "zip': length mismatch"
