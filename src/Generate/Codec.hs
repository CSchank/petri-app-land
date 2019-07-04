{-# LANGUAGE OverloadedStrings #-}

module Generate.Codec where

import Types (Language(..), DocTypeT(..), TypeT(..), CustomT(..))
import qualified Data.Map as M
import qualified Data.Text as T

--generateCodec :: ClientServerApp -> 



generateEncoder :: Language -> CustomT -> T.Text
generateEncoder l (CustomT name edts) =
    let
        delim = if l == Haskell then "\\0" else "\\u{0000}"

        indtTxts indt txts = map (indtTxt indt) txts
        indtTxt indt txt = T.concat [T.replicate (4*indt) " ", txt]

        (.:.) :: T.Text -> T.Text -> T.Text
        (.:.)  a b = T.concat [a, if l == Haskell then ":" else "::", b]

        (.::.) :: T.Text -> T.Text -> T.Text
        (.::.) a b = T.concat [a, if l == Haskell then " :: " else " : ", b]

        encodeEt :: Int -> DocTypeT -> [T.Text]
        encodeEt indt (IntRangeT low high,n,_) = 
            indtTxts indt $ [T.concat[n, "Txt = encodeInt (",T.pack $ show low,") (",T.pack $ show high,") ",n]
                            ]
        encodeEt indt (FloatRangeT low high precision,n,_) =
            let
                pLow = T.pack $ show (round $ low*10^precision)
                pHigh = T.pack $ show (round $ high*10^precision)
                pPrec = T.pack $ show (10^precision)
            in
                indtTxts indt $ [T.concat[n,"Txt"," = encodeInt (",pLow,") (",pHigh,") (round <| (",n,")*(",pPrec,"))"]
                            ]
        encodeEt indt (StringT, n, _) =
            indtTxts indt $ [T.concat [n, if l == Haskell then "Txt = T.pack " else "Txt = ",n]]
        encodeEt indt (SizedStringT size, n, _) =
            error "Not implemented yet"
        encodeEt indt (PairT (et0,n0,d0) (et1,n1,d1), n, _) =
            let
                indtTxt = T.pack $ show indt
            in
            indtTxts indt   [T.concat[n, "Txt ="]
                            ,"    let"
                            ,T.concat["        (fst",indtTxt,",snd",indtTxt,") = ",n]] ++
                            encodeEt (indt+2) (et0,T.concat["fst",indtTxt],d0) ++
                            encodeEt (indt+2) (et1,T.concat["snd",indtTxt],d1) ++
            indtTxts indt   ["    in"
                            ,T.concat ["        tConcat [fst",indtTxt,"Txt,\"",delim,"\",snd",indtTxt,"Txt]"]
                            ]
        encodeEt indt (TripleT (et0,n0,d0) (et1,n1,d1) (et2,n2,d2), n, _) =
            let
                indtTxt = T.pack $ show indt
            in
            indtTxts indt $ [T.concat[n, "Txt ="]
                            ,"    let"
                            ,T.concat["        (fst",indtTxt,",snd",indtTxt,",thd",indtTxt,") = ",n]
                            ,T.unlines $ encodeEt 2 (et0,T.concat["fst",indtTxt],d0)
                            ,T.unlines $ encodeEt 2 (et1,T.concat["snd",indtTxt],d1)
                            ,T.unlines $ encodeEt 2 (et2,T.concat["thd",indtTxt],d2)
                            ,"    in"
                            ,T.concat ["        tConcat[fst",indtTxt,"Txt,\"",delim,"\",snd",indtTxt,"Txt,\"",delim,"\",thd",indtTxt,"Txt]"]
                            ]
        encodeEt indt (ListT (et, etn, etd), n, _) =
            indtTxts indt   [T.concat[n, "Txt ="]
                            ,"    let"
                            ,T.concat["        encode",n,"_ _ (str",T.pack $ show indt,",",n,"List) ="]
                            ,T.concat["            case ",n,"List of"]
                            ,T.concat["                ",etn," " .:. " rest ->"]
                            ,"                    let"] ++
                            (encodeEt (indt+6) (et, etn, etd)) ++
            indtTxts indt   ["                    in"
                            ,T.concat["                        (tConcat [str",T.pack $ show indt,",\"",delim,"\",",etn,"Txt], rest)"]
                            ,T.concat["                [] -> (str",T.pack $ show indt,",",n,"List)"]
                            ,T.concat["        encode",n," ls ="]
                            ,T.concat["            lFoldl encode",n,"_ (\"\",ls) (lRange 0 (lLength ",n,"))"]
                            ,"    in"
                            ,T.concat ["        tConcat [encodeInt 0 16777216 <| lLength ",n,", pFst <| encode",n," ",n,"]"]
                            ]
        encodeEt indt (DictT etd0 etd1, n, _) =
            indtTxts indt $ T.concat [n,"AsList = Dict.toList ",n] :
                encodeEt 0 (ListT (PairT etd0 etd1,"keyValuePairs",""),T.concat[n,"AsList"],"")
        encodeEt indt (TypeT name, n, _) =
            indtTxts indt $ [T.concat[n,"Txt = encode",name," ",n]
                            ]
        encodeEt indt (MaybeT (et, etn, etd), n, _) =
            indtTxts indt  [T.concat[n, "Txt ="]
                            ,T.concat["    case ",n, " of"]
                            ,T.concat["        Just ",etn," ->"]
                            ,         "            let"] ++
                            encodeEt (indt+4) (et,etn,etd)
                            ++
            indtTxts indt   [T.concat["            in tConcat [\"J\",\"",delim,"\",",etn,"Txt]"]
                            ,         "        Nothing -> \"N\""
                            ]
        encodeEt indt (BoolT, n, _) =
            indtTxts indt $ [T.concat[n,"Txt = if ",n," then \"T\" else \"F\""]
                            ]
        encodeEt indt (WildcardTypeT _, _, _) =
            error "Wildcard cannot be serialized."
        encodeEt indt (ResultT edt0@(et0,etn0,etd0) edt1@(et1,etn1,etd1), n, _) =
            indtTxts indt $ [T.concat[n, "Txt ="]
                            ,T.concat["    case ", n, " of"]
                            ,T.concat["        Result.Err ",etn0," ->"]
                            ,         "            let"]++
                            (encodeEt indt edt0)++
                            [T.concat["            in tConcat [\"Err\",\"",delim,"\",",etn0,"Txt]"]
                            ,T.concat["        Result.Ok ",etn1," ->"]
                            ,         "            let"]++
                            (encodeEt indt edt1) ++
                            [T.concat["            in tConcat [\"Ok\",\"",delim,"\",",etn1,"Txt]"]
                            ]
        encodeEt indt (ExistingT _ _, _, _) =
            error "ExistingT serialization not supported. They should only be used for place states and client- and server-only messages."
        encodeEt indt (EmptyT, _, _) = 
            error "EmptyT serialization not supported. They should only be used for place states and client- and server-only messages."

        cases = map (\(constrName,dt) -> 
                        T.concat ["        ",constrName,T.concat $ map (\(et,name,desc) -> T.concat[" ", name]) dt," -> "
                                ,if length dt > 0 then 
                                    T.concat ["\n            let\n"
                                            ,T.unlines $ concat $ map (encodeEt 4) dt
                                            ,"            in"] 
                                 else ""
                                ,"\n                tConcat [\"",constrName
                                ,if length dt > 0 then T.concat[delim,"\", "] else "\""
                                ,T.intercalate (T.concat[",\"",delim,"\","]) $ 
                                        map (\(et,name,desc) -> case et of 
                                                                    DictT _ _ -> T.concat [name, "AsListTxt"]
                                                                    _           -> T.concat [name, "Txt"]
                                        
                                            ) dt, "]"
                                ]) edts
        fullTxt = T.unlines 
                    [T.concat["encode",name .::. name,if l == Haskell then " -> T.Text" else " -> String"]
                    ,T.concat["encode",name," ",T.toLower $ name," = "]
                    ,T.concat["    case ",T.toLower $ name, " of"]
                    ,T.unlines cases
                    ]
    in
        fullTxt

generateDecoder :: Language -> CustomT -> T.Text
generateDecoder l (CustomT name edts) =
    let
        typeSig = if l == Haskell then T.concat["decode",name, " " .::. " (Result T.Text a, [T.Text]) -> (Result T.Text ",name,", [T.Text])"]
                  else      T.concat["decode",name, " " .::. " (Result String a, List String) -> (Result String ",name,", List String)"]


        indtTxts indt txts = map (indtTxt indt) txts
        indtTxt indt txt = T.concat [T.replicate (4*indt) " ", txt]

        (.:.) :: T.Text -> T.Text -> T.Text
        (.:.)  a b = T.concat [a, if l == Haskell then ":" else "::", b]

        (.::.) :: T.Text -> T.Text -> T.Text
        (.::.) a b = T.concat [a, if l == Haskell then " :: " else " : ", b]

        decodeEt :: Int -> DocTypeT -> [T.Text]
        decodeEt indt (IntRangeT low high,n,_) = 
            indtTxts indt $ [T.concat["\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,T.concat["        (case l",T.pack $ show indt," of"]
                            ,T.concat["            (",n,"Txt " .:. " ll",T.pack $ show indt,") -> ","(decodeInt (",T.pack $ show low,") (",T.pack $ show high,") ",n,"Txt |> Result.andThen Ok,ll",T.pack $ show indt,")"]
                            ,T.concat["            [] -> (Err \"Ran out of string to process while parsing ",name,"\",[]))"]
                            ]
        decodeEt indt (FloatRangeT low high precision,n,_) = 
            indtTxts indt $ [T.concat["\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,T.concat["        (case l",T.pack $ show indt," of"]
                            ,T.concat["            (",n,"Txt ".:. " ll",T.pack $ show indt,") -> ","(decodeInt (",T.pack $ show $ round (low*10^precision),") (",T.pack $ show $ round (high*10^precision),") ",n,"Txt |> Result.andThen (\\",n,"Res -> Ok <| toFloat ",n,"Res / (",T.pack $ show (10^precision),")),ll",T.pack $ show indt,")"]
                            ,T.concat["            [] -> (Err \"Ran out of string to process while parsing ",name,"\",[]))"]
                            ]
        decodeEt indt (StringT, n, _) =
            indtTxts indt $ [T.concat["\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,T.concat["    decodeString l",T.pack $ show indt]
                            ]
        decodeEt indt (SizedStringT size, n, _) =
            error "Not implemented yet"
        decodeEt indt (PairT (et0,n0,d0) (et1,n1,d1), n, _) =
            indtTxts indt $ [T.concat["\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,"    let"
                            --parse first part of tuple
                            ,T.concat["        (",n0,",lf",T.pack $ show indt,") ="]
                            ,T.concat["            case l",T.pack $ show indt," of"]
                            ,T.concat["                (",n,"Txt " .:. " llf",T.pack $ show indt,") ->"]
                            ,T.concat["                     (Err \"\",l",T.pack $ show indt,") |>"]
                            ,T.unlines $ decodeEt (indt+5) (et0,n0,d0)
                            ,T.concat["                [] -> (Err \"Ran out of string to process while parsing ",name,"\",[])"]
                            --parse second part of tuple
                            ,T.concat["        (",n1,",ls",T.pack $ show indt,") ="]
                            ,T.concat["            case lf",T.pack $ show indt," of"]
                            ,T.concat["                (",n,"Txt " .:. " lls",T.pack $ show indt,") ->"]
                            ,T.concat["                     (Err \"\",lf",T.pack $ show indt,") |>"]
                            ,T.unlines $ decodeEt (indt+5) (et1,n1,d1)
                            ,T.concat["                [] -> (Err \"Ran out of string to process while parsing ",name,"\",[])"]
                            ,T.concat["    in (Result.map2 (\\rff",T.pack $ show indt," rss",T.pack $ show indt," -> (rff",T.pack $ show indt,",rss",T.pack $ show indt,")) ", n0," ",n1,",ls",T.pack $ show indt,")"]
                            ]
        decodeEt indt (TripleT (et0,n0,d0) (et1,n1,d1) (et2,n2,d2), n, _) =
            indtTxts indt $ [T.concat["\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,"    let"
                            --parse first part of tuple
                            ,T.concat["        (",n0,",lf",T.pack $ show indt,") ="]
                            ,T.concat["            case l",T.pack $ show indt," of"]
                            ,T.concat["                (",n,"Txt " .:. " llf",T.pack $ show indt,") ->"]
                            ,T.concat["                     (\"\",l",T.pack $ show indt,") |>"]
                            ,T.unlines $ decodeEt (indt+5) (et0,n0,d0)
                            ,T.concat["                [] -> (Err \"Ran out of string to process while parsing ",name,"\",[])"]
                            --parse second part of tuple
                            ,T.concat["        (",n1,",ls",T.pack $ show indt,") ="]
                            ,T.concat["            case lf",T.pack $ show indt," of"]
                            ,T.concat["                (",n,"Txt " .:. " lls",T.pack $ show indt,") ->"]
                            ,T.concat["                     (\"\",lf",T.pack $ show indt,") |>"]
                            ,T.unlines $ decodeEt (indt+5) (et1,n1,d1)
                            ,T.concat["                [] -> (Err \"Ran out of string to process while parsing ",name,"\",[])"]
                            --parse third part of tuple
                            ,T.concat["        (",n2,",lt",T.pack $ show indt,") ="]
                            ,T.concat["            case ls",T.pack $ show indt," of"]
                            ,T.concat["                (",n,"Txt " .:. " lls",T.pack $ show indt,") ->"]
                            ,T.concat["                     (\"\",ls",T.pack $ show indt,") |>"]
                            ,T.unlines $ decodeEt (indt+5) (et2,n2,d2)
                            ,T.concat["                [] -> (Err \"Ran out of string to process while parsing ",name,"\",[])"]
                            ,T.concat["    in (Result.map3 (\\rff",T.pack $ show indt," rss",T.pack $ show indt," rtt",T.pack $ show indt," -> (rff",T.pack $ show indt,",rss",T.pack $ show indt,",rtt",T.pack $ show indt,")) ", n0," ",n1," ",n2,",lt",T.pack $ show indt,")"]
                            ]
        decodeEt indt (ListT etd, n, _) =
            indtTxts indt $ [T.concat["\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,T.concat ["(",T.strip $ T.intercalate "\n" $ decodeEt (indt+1) etd,") |>"]
                            ,T.concat["    decodeList l",T.pack $ show indt]
                            ]
        decodeEt indt (DictT etd0 etd1, n, _) =
            indtTxts indt $ [T.strip $ T.unlines $ decodeEt indt (ListT (PairT etd0 etd1,T.concat[n,"KeyValPair"],""),T.concat[n,"AsList"],"")
                            ,"|> decodeDict"
                            ]
                            
        decodeEt indt (TypeT name, n, _) =
            indtTxts indt $ [T.concat["\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"] 
                            ,T.concat["    decode",name," (r",T.pack $ show (indt-1),",l",T.pack $ show indt,")"]
                            ]
        decodeEt indt (MaybeT etd, n, _) =
            indtTxts indt $ [T.concat["\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,T.concat ["(",T.strip $ T.intercalate "\n" $ decodeEt (indt+1) etd,") |>"]
                            ,T.concat["    decodeMaybe l",T.pack $ show indt]
                            ]
        decodeEt indt (BoolT, n, _) =
            indtTxts indt $ [T.concat["\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,T.concat["    decodeBool l",T.pack $ show indt]
                            ]
        decodeEt indt (WildcardTypeT _, _, _) =
            error "Wildcard cannot be serialized."
        decodeEt indt (ResultT edt0 edt1, n, _) =
            indtTxts indt $ [T.concat["\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,"    let"
                            ,T.concat ["        decodeErr",T.pack $ show indt," ="]]++
                            (decodeEt (indt+1) edt0) ++
                            [T.concat ["        decodeOk",T.pack $ show indt," ="]]++
                            (decodeEt (indt+1) edt1) ++
                            ["    in"
                            ,T.concat["        decodeResult decodeErr",T.pack $ show indt," decodeOk",T.pack $ show indt," l",T.pack $ show indt]
                            ]
        decodeEt indt (ExistingT _ _, _, _) =
            error "ExistingT serialization not supported. They should only be used for place states and client- and server-only messages."
        decodeEt indt (EmptyT, _, _) = 
            error "EmptyT serialization not supported. They should only be used for place states and client- and server-only messages."
        decoderEt _ _ = error ""
        cases = map (\(constrName,dt) -> 
                        T.concat [ "        (\"",constrName, "\"", " " .:. " rest) ->"
                                ,"\n            (Err \"\",rest) |> \n"
                                ,T.unlines $ concatMap (\(n,et) -> (decodeEt (4+n) et) ++ indtTxts (4+n) [" |>"]) $ zip [0..] dt
                                ,indtTxt (5 + length dt) $ T.concat ["(\\(r"
                                                                    ,T.pack $ show $ length dt + 3,",l",T.pack $ show $ length dt + 4,") -> ("
                                                                    ,if length dt > 0 && l == Haskell then 
                                                                        T.concat ["Result.map",if length dt > 1 then T.pack $ show $ length dt else ""] 
                                                                        else if length dt > 0 && l == Elm then
                                                                        T.concat ["rMap",if length dt > 1 then T.pack $ show $ length dt else ""] 
                                                                        else "Ok <|"," "
                                                                    ,constrName," "
                                                                    ,T.intercalate " " $ map (\n -> T.pack $ "r" ++ show (n+4)) [0..length dt-1]
                                                                    ,",l",T.pack $ show $ length dt + 4,"))"]
                                ]) edts
    in
        T.unlines [typeSig
                  ,T.concat["decode",name," (_,",T.toLower $ name,"Txts) = "]
                  ,T.concat["    case ",T.toLower $ name, "Txts of"]
                  ,T.unlines cases
                  ,T.concat["        _ -> (Err <| tConcat [\"Incorrect input, could not decode value of type ",name," from string \\\"\", tConcat ", T.toLower $ name,"Txts, \"\\\"\"],[])"]
                  ]