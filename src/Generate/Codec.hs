{-# LANGUAGE OverloadedStrings #-}

module Generate.Codec where

import Types (ElmDocType(..), ElmType(..), ElmCustom(..), ClientStateDiagram, ServerStateDiagram, ClientServerApp, ClientTransition, ServerTransition)
import qualified Data.Map as M
import qualified Data.Text as T

--generateCodec :: ClientServerApp -> 



generateEncoder :: Bool -> ElmCustom -> T.Text
generateEncoder h (ElmCustom name edts) =
    let
        elmDelim = if h then T.pack "\\0" else T.pack "\\u{0000}"

        indtTxts indt txts = map (indtTxt indt) txts
        indtTxt indt txt = T.concat [T.replicate (4*indt) " ", txt]

        (.:.) :: T.Text -> T.Text -> T.Text
        (.:.)  a b = T.concat [a, if h then ":" else "::", b]

        (.::.) :: T.Text -> T.Text -> T.Text
        (.::.) a b = T.concat [a, if h then " :: " else " : ", b]

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
            indtTxts indt $ [T.concat [T.pack n, if h then "Txt = T.pack " else "Txt = ",T.pack n]]
        encodeEt indt (ElmSizedString size, n, _) =
            error "Not implemented yet"--indtTxts indt $ [T.concat[T.pack n, "Txt =",T.pack n]
        encodeEt indt (ElmPair (et0,n0,d0) (et1,n1,d1), n, _) =
            indtTxts indt   [T.concat[T.pack n, "Txt ="]
                            ,"    let"
                            ,T.concat["        (",T.pack n0,",",T.pack n1,") = ",T.pack n]] ++
                            encodeEt (indt+2) (et0,n0,d0) ++
                            encodeEt (indt+2) (et1,n1,d1) ++
            indtTxts indt   ["    in"
                            ,T.concat ["        tConcat [",T.pack n0,"Txt",",",elmDelim,",",T.pack n1,"Txt]"]
                            ]
        encodeEt indt (ElmTriple (et0,n0,d0) (et1,n1,d1) (et2,n2,d2), n, _) =
            indtTxts indt $ [T.concat[T.pack n, "Txt ="]
                            ,"    let"
                            ,T.concat["        (",T.pack n0,",",T.pack n1,",",T.pack n2,") = ",T.pack n]
                            ,T.unlines $ encodeEt (indt+2) (et0,n0,d0)
                            ,T.unlines $ encodeEt (indt+2) (et1,n1,d1)
                            ,T.unlines $ encodeEt (indt+2) (et2,n2,d2)
                            ,"    in"
                            ,T.concat ["        tConcat[",T.pack n0,"Txt,",elmDelim,",",T.pack n1,"Txt,",elmDelim,",",T.pack n2,"Txt]"]
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
                            ,T.concat["                        (str",T.pack $ show indt," ++ \"",elmDelim,"\" ++ ",T.pack etn,"Txt, rest)"]
                            ,T.concat["                [] -> (str",T.pack $ show indt,",",T.pack n,"List)"]
                            ,T.concat["        encode",T.pack n," ls ="]
                            ,T.concat["            List.foldl encode",T.pack n,"_ (\"\",ls) (List.range 0 (List.length ",T.pack n,"))"]
                            ,"    in"
                            ,T.concat ["        tConcat [encodeInt 0 16777216 <| List.length ",T.pack n,", Tuple.first <| encode",T.pack n," ",T.pack n,"]"]
                            ]
        encodeEt indt (ElmDict etd0 etd1, n, _) =
            error "Dictionaries aren't supported yet."
            {-indtTxts indt [
                            T.concat["let"]
                          , T.concat["    ",T.pack n,T.pack $ " = listDictPairs" ++ show indt]
                          ] ++
            encodeEt indt (ElmList (ElmPair etd0 etd1,"dictPair" ++ show indt,""), n, "")-}
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
        cases = map (\(constrName,edt) -> T.concat ["        ",T.pack constrName,T.concat $ map (\(et,name,desc) -> T.pack $ " " ++ name) edt," -> "
                                                    ,if length edt > 0 then T.concat ["\n            let\n"
                                                    ,T.unlines $ concat $ map (encodeEt 4) edt
                                                    ,"            in\n"] else ""
                                                    ,"                tConcat [\"",T.pack constrName,elmDelim,if length edt > 0 then "\", " else "\"",T.intercalate (T.concat[",\"",elmDelim,"\","]) $ map (\(et,name,desc) -> T.pack $ name ++ "Txt") edt, "]"
                                                    ]) edts
        fullTxt = T.unlines 
                    [T.concat["encode",T.pack name .::. T.pack name,if h then " -> T.Text" else " -> String"]
                    ,T.concat["encode",T.pack name," ",T.toLower $ T.pack name," = "]
                    ,T.concat["    case ",T.toLower $ T.pack name, " of"]
                    ,T.unlines cases
                    ]
    in
        fullTxt

generateDecoder :: Bool -> ElmCustom -> T.Text
generateDecoder h (ElmCustom name edts) =
    let
        typeSig = if h then T.concat["decode",T.pack name, " " .::. " (Result T.Text ",T.pack name,", [T.Text]) -> (Result T.Text ",T.pack name,", [T.Text])"]
                  else      T.concat["decode",T.pack name, " " .::. " (Result String ",T.pack name,", List String) -> (Result String ",T.pack name,", List String)"]


        indtTxts indt txts = map (indtTxt indt) txts
        indtTxt indt txt = T.concat [T.replicate (4*indt) " ", txt]

        (.:.) :: T.Text -> T.Text -> T.Text
        (.:.)  a b = T.concat [a, if h then ":" else "::", b]

        (.::.) :: T.Text -> T.Text -> T.Text
        (.::.) a b = T.concat [a, if h then " :: " else " : ", b]

        decodeEt :: Int -> ElmDocType -> [T.Text]
        decodeEt indt (ElmIntRange low high,n,_) = 
            indtTxts indt $ [T.concat["\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,T.concat["        (case l",T.pack $ show indt," of"]
                            ,T.concat["            (",T.pack n,"Txt " .:. " ll",T.pack $ show indt,") -> ","(decodeInt ",T.pack $ show low," ",T.pack $ show high," ",T.pack n,"Txt |> randThen Ok,ll",T.pack $ show indt,")"]
                            ,T.concat["            [] -> (Err \"Ran out of string to process while parsing ",T.pack name,"\",[]))"]
                            ]
        decodeEt indt (ElmFloatRange low high precision,n,_) = 
            indtTxts indt $ [T.concat["\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,T.concat["        (case l",T.pack $ show indt," of"]
                            ,T.concat["            (",T.pack n,"Txt ".:. " ll",T.pack $ show indt,") -> ","(decodeInt ",T.pack $ show $ round (low*10^precision)," ",T.pack $ show $ round (high*10^precision)," ",T.pack n,"Txt |> randThen (\\",T.pack n,"Res -> Ok <| toFloat ",T.pack n,"Res / ",T.pack $ show (10^precision),"),ll",T.pack $ show indt,")"]
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
                            ,T.concat["    in (rMap2 (\\rff",T.pack $ show indt," rss",T.pack $ show indt," -> (rff",T.pack $ show indt,",rss",T.pack $ show indt,")) ", T.pack n0," ",T.pack n1,",ls",T.pack $ show indt,")"]
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
                            ,T.unlines $ decodeEt (indt+5) (et1,n1,d1)
                            ,T.concat["                [] -> (Err \"Ran out of string to process while parsing ",T.pack name,"\",[])"]
                            ,T.concat["    in (rMap3 (\\rff",T.pack $ show indt," rss",T.pack $ show indt," rtt",T.pack $ show indt," -> (rff",T.pack $ show indt,",rss",T.pack $ show indt,",rtt",T.pack $ show indt,")) ", T.pack n0," ",T.pack n1," ",T.pack n2,",lt",T.pack $ show indt,")"]
                            ]
        decodeEt indt (ElmList etd, n, _) =
            indtTxts indt $ [T.concat["\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,T.concat ["(",T.strip $ T.intercalate "\n" $ decodeEt (indt+1) etd,") |>"]
                            ,T.concat["    decodeList l",T.pack $ show indt]
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
        {-decodeEt indt (ElmMaybe etd, n, _) =
            indtTxts indt $ [T.concat["(\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,T.concat["            case l",T.pack $ show indt," of"]
                            ,T.concat["                (",T.pack n,"Txt " .:. " llf",T.pack $ show indt,") ->"]
                            ,T.concat["                     decodeMaybe l",T.pack $ show indt," ((\"\",l",T.pack $ show indt,") |>"]
                            ,                             T.unlines $ decodeEt (indt+5) etd,")"
                            ,T.concat["                [] -> (Err \"Ran out of string to process while parsing ",T.pack name,"\",[]))"]
                            ]-}
        cases = map (\(constrName,edt) -> T.concat [ T.pack "        (\"",T.pack constrName, T.pack "\"", " " .:. " rest) ->"
                                                   ,"\n            (Err \"\",rest) |> \n"
                                                   ,T.unlines $ concat $ map (\(n,et) -> (decodeEt (4+n) et) ++ indtTxts (4+n) [" |>"]) $ zip [0..] edt
                                                   ,indtTxt (5 + length edt) $ T.concat ["(\\(r"
                                                                                        ,T.pack $ show $ length edt + 3,",l",T.pack $ show $ length edt + 4,") -> (",if length edt > 0 then T.concat ["rMap",T.pack $ show $ length edt] else "Ok <|"," ",T.pack constrName," ",T.intercalate " " $ map (\n -> T.pack $ "r" ++ show (n+4)) [0..length edt-1],",l",T.pack $ show $ length edt + 4,"))"]
                                                   ]) edts
    in
        T.unlines [typeSig
                  ,T.concat["decode",T.pack name," (lastRes,",T.toLower $ T.pack name,"Txts) = "]
                  ,T.concat["    case ",T.toLower $ T.pack name, "Txts of"]
                  ,T.unlines cases
                  ,T.concat["        _ -> (Err <| tConcat [\"Incorrect input, could not decode value of type ",T.pack name," from string \\\"\", tConcat ", T.toLower $ T.pack name,"Txts, \"\\\"\"],[])"]
                  ]