{-# LANGUAGE OverloadedStrings #-}

module Generate.AllOf where

import Types
import Data.Char (ord,chr)
import Generate.Types
import qualified Data.Text as T

generateAllOf :: Bool -> Int -> T.Text
generateAllOf h n =
    let
        (.::.) = if h then " :: " else " : "
        nTxt = T.pack $ show n
        name = T.concat["AllOf",nTxt]
        constr = (T.unpack name,[(ElmWildcardType (i2A n),(i2A n),"") | n <- [0..n-1]])
        typ = generateType True False [] $ ElmCustom (T.unpack name) [constr]
        decl = T.concat["unwrap ",T.intercalate " " $ map (\n -> T.concat["f",T.pack $ show n]) [0..n-1], " wl ",generatePattern constr," ="]
        allOf = T.concat["AllOf",nTxt," ",T.intercalate " " $ map (T.pack . i2A) [0..n-1]]
        oneCase n = 
            let
                nTxt = T.pack $ show n
                arg = T.pack $ i2A n
            in
                T.concat["f",nTxt," ",arg]
    in
        T.unlines
            [
                T.concat["module Static.AllOf.",name," where"]
            ,   typ
            ,   T.concat["unwrap",(.::.),T.intercalate " -> " (map (\n -> T.concat["(",T.pack $ i2A n, " -> ","a0)"]) [0..n-1])," -> ([a0] -> a0) -> ",allOf," -> a0"]
            ,   decl
            ,   T.concat  ["    wl [",T.intercalate "," $ map oneCase [0..n-1],"]"]
            ]

i2A :: Int -> String
i2A n =
    if n < 26 then
        chr (97 + n `mod` 26) : ""
    else
        chr (97 + n `mod` 26) : i2A (n `div` 26)