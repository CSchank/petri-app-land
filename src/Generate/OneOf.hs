{-# LANGUAGE OverloadedStrings #-}

module Generate.OneOf where

import Types
import Data.Char (ord,chr)
import Generate.Types
import qualified Data.Text as T

generateOneOf :: Bool -> Int -> T.Text
generateOneOf h n =
    let
        (.::.) = if h then " :: " else " : "
        nTxt = T.pack $ show n
        name = T.concat["OneOf",nTxt]
        constrs = map (\n -> ("Op"++show n,[(WildcardTypeT (i2A n),"","")])) [0..n-1]
        typ = generateType Haskell False [] $ CustomT (T.unpack name) $ constrs
        decl = T.concat["unwrap ",T.intercalate " " $ map (\n -> T.concat["f",T.pack $ show n]) [0..n-1], " oneOf ="]
        oneOf = T.concat["OneOf",nTxt," ",T.intercalate " " $ map (T.pack . i2A) [0..n-1]]
        oneCase n = 
            let
                nTxt = T.pack $ show n
                arg = T.pack $ i2A n ++ "1"
            in
                T.concat["        Op",nTxt," ",arg," -> f",T.pack $ show n," ",arg]
    in
        T.unlines
            [
                T.concat["module Static.OneOf.",name," where"]
            ,   typ
            ,   T.concat["unwrap",(.::.),T.intercalate " -> " (map (\n -> T.concat["(",T.pack $ i2A n, " -> ","a0)"]) [0..n-1])," -> ",oneOf," -> a0"]
            ,   decl
            ,   "    case oneOf of"
            ,   T.unlines $ map oneCase [0..n-1]
            ]

i2A :: Int -> String
i2A n =
    if n < 26 then
        chr (97 + n `mod` 26) : ""
    else
        chr (97 + n `mod` 26) : i2A (n `div` 26)