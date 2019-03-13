{-# LANGUAGE OverloadedStrings #-}

module Generate.Codec where

import Types (Language(..), ElmDocType(..), ElmType(..), ElmCustom(..))
import qualified Data.Map as M
import qualified Data.Text as T

--generateCodec :: ClientServerApp -> 



generateEncoder :: Language -> ElmCustom -> T.Text
generateEncoder l (ElmCustom name edts) =
    let
        elmDelim = if l == Haskell then T.pack "\\0" else T.pack "\\u{0000}"

        indtTxts indt txts = map (indtTxt indt) txts
        indtTxt indt txt = T.concat [T.replicate (4*indt) " ", txt]

        (.:.) :: T.Text -> T.Text -> T.Text
        (.:.)  a b = T.concat [a, if l == Haskell then ":" else "::", b]

        (.::.) :: T.Text -> T.Text -> T.Text
        (.::.) a b = T.concat [a, if l == Haskell then " :: " else " : ", b]

        encodeEt :: Int -> ElmDocType -> [T.Text]
        encodeEt indt (ElmIntRange low high,n,_) = 
            indtTxts indt $ [T.concat[T.pack n, "Txt = encodeInt ",T.pack $ show low," ",T.pack $ show high," ",T.pack n]
                            ]
        encodeEt indt (ElmFloatRange low high precision,n,_) =
            let
                pLow = T.pack $ show (round $ low*10^precision)
                pHigh = T.pack $ show (round $ high*10^precision)
                pPrec = T.pack $ show (10^precision)
            in
                indtTxts indt $ [T.concat[T.pack $ n ++ "Txt"," = encodeInt ",pLow," ",pHigh," (round <| (",T.pack n,"-",pLow,")*",pPrec ,")"]
                            ]
        encodeEt indt (ElmString, n, _) =
            indtTxts indt $ [T.concat [T.pack n, if l == Haskell then "Txt = T.pack " else "Txt = ",T.pack n]]
        encodeEt indt (ElmSizedString size, n, _) =
            error "Not implemented yet"--indtTxts indt $ [T.concat[T.pack n, "Txt =",T.pack n]
        encodeEt indt (ElmPair (et0,n0,d0) (et1,n1,d1), n, _) =
            let
                indtTxt = T.pack $ show indt
            in
            indtTxts indt   [T.concat[T.pack n, "Txt ="]
                            ,"    let"
                            ,T.concat["        (fst",indtTxt,",snd",indtTxt,") = ",T.pack n]] ++
                            encodeEt (indt+2) (et0,"fst"++T.unpack indtTxt,d0) ++
                            encodeEt (indt+2) (et1,"snd"++T.unpack indtTxt,d1) ++
            indtTxts indt   ["    in"
                            ,T.concat ["        tConcat [fst",indtTxt,"Txt,\"",elmDelim,"\",snd",indtTxt,"Txt]"]
                            ]
        encodeEt indt (ElmTriple (et0,n0,d0) (et1,n1,d1) (et2,n2,d2), n, _) =
            let
                indtTxt = T.pack $ show indt
            in
            indtTxts indt $ [T.concat[T.pack n, "Txt ="]
                            ,"    let"
                            ,T.concat["        (fst",indtTxt,",snd",indtTxt,",thd",indtTxt,") = ",T.pack n]
                            ,T.unlines $ encodeEt 2 (et0,"fst"++T.unpack indtTxt,d0)
                            ,T.unlines $ encodeEt 2 (et1,"snd"++T.unpack indtTxt,d1)
                            ,T.unlines $ encodeEt 2 (et2,"thd"++T.unpack indtTxt,d2)
                            ,"    in"
                            ,T.concat ["        tConcat[fst",indtTxt,"Txt,\"",elmDelim,"\",snd",indtTxt,"Txt,\"",elmDelim,"\",thd",indtTxt,"Txt]"]
                            ]
        encodeEt indt (ElmList (et, etn, etd), n, _) =
            indtTxts indt   [T.concat[T.pack n, "Txt ="]
                            ,"    let"
                            ,T.concat["        encode",T.pack n,"_ _ (str",T.pack $ show indt,",",T.pack n,"List) ="]
                            ,T.concat["            case ",T.pack n,"List of"]
                            ,T.concat["                ",T.pack etn," " .:. " rest ->"]
                            ,"                    let"] ++
                            (encodeEt (indt+6) (et, etn, etd)) ++
            indtTxts indt   ["                    in"
                            ,T.concat["                        (tConcat [str",T.pack $ show indt,",\"",elmDelim,"\",",T.pack etn,"Txt], rest)"]
                            ,T.concat["                [] -> (str",T.pack $ show indt,",",T.pack n,"List)"]
                            ,T.concat["        encode",T.pack n," ls ="]
                            ,T.concat["            lFoldl encode",T.pack n,"_ (\"\",ls) (lRange 0 (lLength ",T.pack n,"))"]
                            ,"    in"
                            ,T.concat ["        tConcat [encodeInt 0 16777216 <| lLength ",T.pack n,", pFst <| encode",T.pack n," ",T.pack n,"]"]
                            ]
        encodeEt indt (ElmDict etd0 etd1, n, _) =
            indtTxts indt $ T.concat [T.pack n,"AsList = Dict.toList ",T.pack n] :
                encodeEt 0 (ElmList (ElmPair etd0 etd1,"keyValuePairs",""),n++"AsList","")
        encodeEt indt (ElmType name, n, _) =
            indtTxts indt $ [T.concat[T.pack n,"Txt = encode",T.pack name," ",T.pack n]
                            ]
        encodeEt indt (ElmMaybe (et, etn, etd), n, _) =
            indtTxts indt $ [T.concat[T.pack n, "Txt ="]
                            ,T.concat["    case ", T.pack n, " of"]
                            ,T.concat["        Just ",T.pack etn," ->"]
                            ,         "            let"
                            ,T.unlines $ encodeEt indt (et,etn,etd)
                            ,T.concat["            in tConcat [\"J\",\"",elmDelim,"\",",T.pack etn,"Txt]"]
                            ,         "        Nothing -> \"N\""
                            ]
        encodeEt indt (ElmBool, n, _) =
            indtTxts indt $ [T.concat[T.pack n,"Txt = if ",T.pack n," then \"T\" else \"F\""]
                            ]
        encodeEt indt (ElmWildcardType _, _, _) =
            error "Wildcard cannot be serialized."
        encodeEt indt (ElmResult edt0@(et0,etn0,etd0) edt1@(et1,etn1,etd1), n, _) =
            indtTxts indt $ [T.concat[T.pack n, "Txt ="]
                            ,T.concat["    case ", T.pack n, " of"]
                            ,T.concat["        Result.Err ",T.pack etn0," ->"]
                            ,         "            let"]++
                            (encodeEt indt edt0)++
                            [T.concat["            in tConcat [\"Err\",\"",elmDelim,"\",",T.pack etn0,"Txt]"]
                            ,T.concat["        Result.Ok ",T.pack etn1," ->"]
                            ,         "            let"]++
                            (encodeEt indt edt1) ++
                            [T.concat["            in tConcat [\"Ok\",\"",elmDelim,"\",",T.pack etn1,"Txt]"]
                            ]
        encodeEt indt (ElmExisting _ _, _, _) =
            error "ElmExisting serialization not supported. They should only be used for place states and client- and server-only messages."
        encodeEt indt (ElmEmpty, _, _) = 
            error "ElmEmpty serialization not supported. They should only be used for place states and client- and server-only messages."

        cases = map (\(constrName,edt) -> 
                        T.concat ["        ",T.pack constrName,T.concat $ map (\(et,name,desc) -> T.pack $ " " ++ name) edt," -> "
                                ,if length edt > 0 then 
                                    T.concat ["\n            let\n"
                                            ,T.unlines $ concat $ map (encodeEt 4) edt
                                            ,"            in"] 
                                 else ""
                                ,"\n                tConcat [\"",T.pack constrName
                                ,if length edt > 0 then T.concat[elmDelim,"\", "] else "\""
                                ,T.intercalate (T.concat[",\"",elmDelim,"\","]) $ 
                                        map (\(et,name,desc) -> case et of 
                                                                    ElmDict _ _ -> T.pack $ name ++ "AsListTxt"
                                                                    _           -> T.pack $ name ++ "Txt"
                                        
                                            ) edt, "]"
                                ]) edts
        fullTxt = T.unlines 
                    [T.concat["encode",T.pack name .::. T.pack name,if l == Haskell then " -> T.Text" else " -> String"]
                    ,T.concat["encode",T.pack name," ",T.toLower $ T.pack name," = "]
                    ,T.concat["    case ",T.toLower $ T.pack name, " of"]
                    ,T.unlines cases
                    ]
    in
        fullTxt

generateDecoder :: Language -> ElmCustom -> T.Text
generateDecoder l (ElmCustom name edts) =
    let
        typeSig = if l == Haskell then T.concat["decode",T.pack name, " " .::. " (Result T.Text a, [T.Text]) -> (Result T.Text ",T.pack name,", [T.Text])"]
                  else      T.concat["decode",T.pack name, " " .::. " (Result String a, List String) -> (Result String ",T.pack name,", List String)"]


        indtTxts indt txts = map (indtTxt indt) txts
        indtTxt indt txt = T.concat [T.replicate (4*indt) " ", txt]

        (.:.) :: T.Text -> T.Text -> T.Text
        (.:.)  a b = T.concat [a, if l == Haskell then ":" else "::", b]

        (.::.) :: T.Text -> T.Text -> T.Text
        (.::.) a b = T.concat [a, if l == Haskell then " :: " else " : ", b]

        decodeEt :: Int -> ElmDocType -> [T.Text]
        decodeEt indt (ElmIntRange low high,n,_) = 
            indtTxts indt $ [T.concat["\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,T.concat["        (case l",T.pack $ show indt," of"]
                            ,T.concat["            (",T.pack n,"Txt " .:. " ll",T.pack $ show indt,") -> ","(decodeInt (",T.pack $ show low,") (",T.pack $ show high,") ",T.pack n,"Txt |> Result.andThen Ok,ll",T.pack $ show indt,")"]
                            ,T.concat["            [] -> (Err \"Ran out of string to process while parsing ",T.pack name,"\",[]))"]
                            ]
        decodeEt indt (ElmFloatRange low high precision,n,_) = 
            indtTxts indt $ [T.concat["\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,T.concat["        (case l",T.pack $ show indt," of"]
                            ,T.concat["            (",T.pack n,"Txt ".:. " ll",T.pack $ show indt,") -> ","(decodeInt (",T.pack $ show $ round (low*10^precision),") (",T.pack $ show $ round (high*10^precision),") ",T.pack n,"Txt |> Result.andThen (\\",T.pack n,"Res -> Ok <| toFloat ",T.pack n,"Res / (",T.pack $ show (10^precision),")),ll",T.pack $ show indt,")"]
                            ,T.concat["            [] -> (Err \"Ran out of string to process while parsing ",T.pack name,"\",[]))"]
                            ]
        decodeEt indt (ElmString, n, _) =
            indtTxts indt $ [T.concat["\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,T.concat["    decodeString l",T.pack $ show indt]
                            ]
        decodeEt indt (ElmSizedString size, n, _) =
            error "Not implemented yet"--indtTxts indt $ [T.concat[T.pack n, "Txt =",T.pack n]
        decodeEt indt (ElmPair (et0,n0,d0) (et1,n1,d1), n, _) =
            indtTxts indt $ [T.concat["\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,"    let"
                            --parse first part of tuple
                            ,T.concat["        (",T.pack n0,",lf",T.pack $ show indt,") ="]
                            ,T.concat["            case l",T.pack $ show indt," of"]
                            ,T.concat["                (",T.pack n,"Txt " .:. " llf",T.pack $ show indt,") ->"]
                            ,T.concat["                     (Err \"\",l",T.pack $ show indt,") |>"]
                            ,T.unlines $ decodeEt (indt+5) (et0,n0,d0)
                            ,T.concat["                [] -> (Err \"Ran out of string to process while parsing ",T.pack name,"\",[])"]
                            --parse second part of tuple
                            ,T.concat["        (",T.pack n1,",ls",T.pack $ show indt,") ="]
                            ,T.concat["            case lf",T.pack $ show indt," of"]
                            ,T.concat["                (",T.pack n,"Txt " .:. " lls",T.pack $ show indt,") ->"]
                            ,T.concat["                     (Err \"\",lf",T.pack $ show indt,") |>"]
                            ,T.unlines $ decodeEt (indt+5) (et1,n1,d1)
                            ,T.concat["                [] -> (Err \"Ran out of string to process while parsing ",T.pack name,"\",[])"]
                            ,T.concat["    in (Result.map2 (\\rff",T.pack $ show indt," rss",T.pack $ show indt," -> (rff",T.pack $ show indt,",rss",T.pack $ show indt,")) ", T.pack n0," ",T.pack n1,",ls",T.pack $ show indt,")"]
                            ]
        decodeEt indt (ElmTriple (et0,n0,d0) (et1,n1,d1) (et2,n2,d2), n, _) =
            indtTxts indt $ [T.concat["\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,"    let"
                            --parse first part of tuple
                            ,T.concat["        (",T.pack n0,",lf",T.pack $ show indt,") ="]
                            ,T.concat["            case l",T.pack $ show indt," of"]
                            ,T.concat["                (",T.pack n,"Txt " .:. " llf",T.pack $ show indt,") ->"]
                            ,T.concat["                     (\"\",l",T.pack $ show indt,") |>"]
                            ,T.unlines $ decodeEt (indt+5) (et0,n0,d0)
                            ,T.concat["                [] -> (Err \"Ran out of string to process while parsing ",T.pack name,"\",[])"]
                            --parse second part of tuple
                            ,T.concat["        (",T.pack n1,",ls",T.pack $ show indt,") ="]
                            ,T.concat["            case lf",T.pack $ show indt," of"]
                            ,T.concat["                (",T.pack n,"Txt " .:. " lls",T.pack $ show indt,") ->"]
                            ,T.concat["                     (\"\",lf",T.pack $ show indt,") |>"]
                            ,T.unlines $ decodeEt (indt+5) (et1,n1,d1)
                            ,T.concat["                [] -> (Err \"Ran out of string to process while parsing ",T.pack name,"\",[])"]
                            --parse third part of tuple
                            ,T.concat["        (",T.pack n2,",lt",T.pack $ show indt,") ="]
                            ,T.concat["            case ls",T.pack $ show indt," of"]
                            ,T.concat["                (",T.pack n,"Txt " .:. " lls",T.pack $ show indt,") ->"]
                            ,T.concat["                     (\"\",ls",T.pack $ show indt,") |>"]
                            ,T.unlines $ decodeEt (indt+5) (et2,n2,d2)
                            ,T.concat["                [] -> (Err \"Ran out of string to process while parsing ",T.pack name,"\",[])"]
                            ,T.concat["    in (Result.map3 (\\rff",T.pack $ show indt," rss",T.pack $ show indt," rtt",T.pack $ show indt," -> (rff",T.pack $ show indt,",rss",T.pack $ show indt,",rtt",T.pack $ show indt,")) ", T.pack n0," ",T.pack n1," ",T.pack n2,",lt",T.pack $ show indt,")"]
                            ]
        decodeEt indt (ElmList etd, n, _) =
            indtTxts indt $ [T.concat["\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,T.concat ["(",T.strip $ T.intercalate "\n" $ decodeEt (indt+1) etd,") |>"]
                            ,T.concat["    decodeList l",T.pack $ show indt]
                            ]
        decodeEt indt (ElmDict etd0 etd1, n, _) =
            indtTxts indt $ [T.strip $ T.unlines $ decodeEt indt (ElmList (ElmPair etd0 etd1,n++"KeyValPair",""),n++"AsList","")
                            ,"|> decodeDict"
                            ]
                            
        decodeEt indt (ElmType name, n, _) =
            indtTxts indt $ [T.concat["\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"] 
                            ,T.concat["    decode",T.pack name," (r",T.pack $ show (indt-1),",l",T.pack $ show indt,")"]
                            ]
        decodeEt indt (ElmMaybe etd, n, _) =
            indtTxts indt $ [T.concat["\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,T.concat ["(",T.strip $ T.intercalate "\n" $ decodeEt (indt+1) etd,") |>"]
                            ,T.concat["    decodeMaybe l",T.pack $ show indt]
                            ]
        decodeEt indt (ElmBool, n, _) =
            indtTxts indt $ [T.concat["\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,T.concat["    decodeBool l",T.pack $ show indt]
                            ]
        decodeEt indt (ElmWildcardType _, _, _) =
            error "Wildcard cannot be serialized."
        decodeEt indt (ElmResult edt0 edt1, n, _) =
            indtTxts indt $ [T.concat["\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,"    let"
                            ,T.concat ["        decodeErr",T.pack $ show indt," ="]]++
                            (decodeEt (indt+1) edt0) ++
                            [T.concat ["        decodeOk",T.pack $ show indt," ="]]++
                            (decodeEt (indt+1) edt1) ++
                            ["    in"
                            ,T.concat["        decodeResult decodeErr",T.pack $ show indt," decodeOk",T.pack $ show indt," l",T.pack $ show indt]
                            ]
        decodeEt indt (ElmExisting _ _, _, _) =
            error "ElmExisting serialization not supported. They should only be used for place states and client- and server-only messages."
        decodeEt indt (ElmEmpty, _, _) = 
            error "ElmEmpty serialization not supported. They should only be used for place states and client- and server-only messages."

        {-decodeEt indt (ElmMaybe etd, n, _) =
            indtTxts indt $ [T.concat["(\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,T.concat["            case l",T.pack $ show indt," of"]
                            ,T.concat["                (",T.pack n,"Txt " .:. " llf",T.pack $ show indt,") ->"]
                            ,T.concat["                     decodeMaybe l",T.pack $ show indt," ((\"\",l",T.pack $ show indt,") |>"]
                            ,                             T.unlines $ decodeEt (indt+5) etd,")"
                            ,T.concat["                [] -> (Err \"Ran out of string to process while parsing ",T.pack name,"\",[]))"]
                            ]-}
        cases = map (\(constrName,edt) -> 
                        T.concat [ T.pack "        (\"",T.pack constrName, T.pack "\"", " " .:. " rest) ->"
                                ,"\n            (Err \"\",rest) |> \n"
                                ,T.unlines $ concatMap (\(n,et) -> (decodeEt (4+n) et) ++ indtTxts (4+n) [" |>"]) $ zip [0..] edt
                                ,indtTxt (5 + length edt) $ T.concat ["(\\(r"
                                                                    ,T.pack $ show $ length edt + 3,",l",T.pack $ show $ length edt + 4,") -> ("
                                                                    ,if length edt > 0 && l == Haskell then 
                                                                        T.concat ["Result.map",if length edt > 1 then T.pack $ show $ length edt else ""] 
                                                                        else if length edt > 0 && l == Elm then
                                                                        T.concat ["rMap",if length edt > 1 then T.pack $ show $ length edt else ""] 
                                                                        else "Ok <|"," "
                                                                    ,T.pack constrName," "
                                                                    ,T.intercalate " " $ map (\n -> T.pack $ "r" ++ show (n+4)) [0..length edt-1]
                                                                    ,",l",T.pack $ show $ length edt + 4,"))"]
                                ]) edts
    in
        T.unlines [typeSig
                  ,T.concat["decode",T.pack name," (_,",T.toLower $ T.pack name,"Txts) = "]
                  ,T.concat["    case ",T.toLower $ T.pack name, "Txts of"]
                  ,T.unlines cases
                  ,T.concat["        _ -> (Err <| tConcat [\"Incorrect input, could not decode value of type ",T.pack name," from string \\\"\", tConcat ", T.toLower $ T.pack name,"Txts, \"\\\"\"],[])"]
                  ]