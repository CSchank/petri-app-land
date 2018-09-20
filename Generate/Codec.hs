{-# LANGUAGE OverloadedStrings #-}

module Generate.Codec where

import Types (ElmDocType(..), ElmType(..), ElmCustom(..), ClientStateDiagram, ServerStateDiagram, ClientServerApp, ClientTransition, ServerTransition)
import qualified Data.Map as M
import qualified Data.Text as T

--generateCodec :: ClientServerApp -> 

elmDelim = T.pack "\\u{0000}"


generateEncoder :: Bool -> ElmCustom -> T.Text
generateEncoder h (ElmCustom name edts) =
    let
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
            indtTxts indt $ [T.concat[T.pack $ n ++ "Txt"," ="]
                            ,"    let"
                            ,T.concat["        low  = ", T.pack $ show low]
                            ,T.concat["        high = ", T.pack $ show high]
                            ,"    in" 
                            ,T.concat["        encodeInt <| round (",T.pack n,"-low)*",T.pack $ show $ 10^precision]
                            ]
        encodeEt indt (ElmString, n, _) =
            indtTxts indt $ [T.concat [T.pack n, "Txt = ",T.pack n]]
        encodeEt indt (ElmSizedString size, n, _) =
            error "Not implemented yet"--indtTxts indt $ [T.concat[T.pack n, "Txt =",T.pack n]
        encodeEt indt (ElmPair (et0,n0,d0) (et1,n1,d1), n, _) =
            indtTxts indt   [T.concat[T.pack n, "Txt ="]
                            ,"    let"
                            ,T.concat["        (",T.pack n0,",",T.pack n1,") = ",T.pack n]] ++
                            encodeEt (indt+2) (et0,n0,d0) ++
                            encodeEt (indt+2) (et1,n1,d1) ++
            indtTxts indt   ["    in"
                            ,T.concat ["        ",T.pack n0,"Txt","++\"",elmDelim,"\"++",T.pack n1,"Txt"]
                            ]
        encodeEt indt (ElmTriple (et0,n0,d0) (et1,n1,d1) (et2,n2,d2), n, _) =
            indtTxts indt $ [T.concat[T.pack n, "Txt ="]
                            ,"    let"
                            ,T.concat["        (",T.pack n0,",",T.pack n1,",",T.pack n2,") = ",T.pack n]
                            ,T.unlines $ encodeEt (indt+2) (et0,n0,d0)
                            ,T.unlines $ encodeEt (indt+2) (et1,n1,d1)
                            ,T.unlines $ encodeEt (indt+2) (et2,n2,d2)
                            ,"    in"
                            ,T.concat ["        ",T.pack n0,"Txt","++\"",elmDelim,"\"++",T.pack n1,"Txt","++\"",elmDelim,"\"++",T.pack n2,"Txt"]
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
                            ,T.concat ["        (encodeInt 0 16777216 <| List.length ",T.pack n,") ++ (Tuple.first <| encode",T.pack n," ",T.pack n,")"]
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
        cases = map (\(constrName,edt) -> T.concat ["        ",T.pack constrName,T.concat $ map (\(et,name,desc) -> T.pack $ " " ++ name) edt," -> "
                                                    ,if length edt > 0 then T.concat ["\n            let\n"
                                                    ,T.unlines $ concat $ map (encodeEt 4) edt
                                                    ,"            in\n"] else ""
                                                    ,"                \"",T.pack constrName,elmDelim,if length edt > 0 then "\" ++ " else "\"",T.intercalate (T.concat["++\"",elmDelim,"\"++"]) $ map (\(et,name,desc) -> T.pack $ name ++ "Txt") edt
                                                    ]) edts
        fullTxt = T.unlines 
                    [T.concat["encode",T.pack name .::. T.pack name," -> String"]
                    ,T.concat["encode",T.pack name," ",T.toLower $ T.pack name," = "]
                    ,T.concat["    case ",T.toLower $ T.pack name, " of"]
                    ,T.unlines cases
                    ]
    in
        fullTxt

generateDecoder :: Bool -> ElmCustom -> T.Text
generateDecoder h (ElmCustom name edts) =
    let
        typeSig = T.concat["decode",T.pack name, " " .::. " (Result String ",T.pack name,", List String) -> (Result String ",T.pack name,", List String)"]

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
                            ,T.concat["            (",T.pack n,"Txt " .:. " ll",T.pack $ show indt,") -> ","(decodeInt ",T.pack $ show low," ",T.pack $ show high," ",T.pack n,"Txt |> randThen (\\",T.pack n,"Res -> Ok <| ",T.pack n,"Res + ",T.pack $ show low,"),ll",T.pack $ show indt,")"]
                            ,T.concat["            [] -> (Err \"Ran out of string to process while parsing ",T.pack name,"\",[]))"]
                            ]
        decodeEt indt (ElmFloatRange low high precision,n,_) = 
            indtTxts indt $ [T.concat["\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,T.concat["        (case l",T.pack $ show indt," of"]
                            ,T.concat["            (",T.pack n,"Txt ".:. " ll",T.pack $ show indt,") -> ","(decodeInt ",T.pack $ show $ round (low*10^precision)," ",T.pack $ show $ round (high*10^precision)," ",T.pack n,"Txt |> randThen (\\",T.pack n,"Res -> Ok <| (toFloat ",T.pack n,"Res + ",T.pack $ show (round (low * 10^precision)),") / ",T.pack $ show (10^precision),"),ll",T.pack $ show indt,")"]
                            ,T.concat["            [] -> (Err \"Ran out of string to process while parsing ",T.pack name,"\",[]))"]
                            ]
        decodeEt indt (ElmString, n, _) =
            indtTxts indt $ [T.concat [T.pack n, " = ",T.pack n,"Txt"]]
        decodeEt indt (ElmSizedString size, n, _) =
            error "Not implemented yet"--indtTxts indt $ [T.concat[T.pack n, "Txt =",T.pack n]
        decodeEt indt (ElmPair (et0,n0,d0) (et1,n1,d1), n, _) =
            indtTxts indt $ [T.concat["(\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
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
                            ,T.concat["    in (rMap2 (\\rff",T.pack $ show indt," rss",T.pack $ show indt," -> (rff",T.pack $ show indt,",rss",T.pack $ show indt,")) ", T.pack n0," ",T.pack n1,",ls",T.pack $ show indt,"))"]
                            ]
        decodeEt indt (ElmTriple (et0,n0,d0) (et1,n1,d1) (et2,n2,d2), n, _) =
            indtTxts indt $ [T.concat["(\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
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
                            ,T.concat["    in (rMap3 (\\rff",T.pack $ show indt," rss",T.pack $ show indt," rtt",T.pack $ show indt," -> (rff",T.pack $ show indt,",rss",T.pack $ show indt,",rtt",T.pack $ show indt,")) ", T.pack n0," ",T.pack n1," ",T.pack n2,",lt",T.pack $ show indt,"))"]
                            ]
        decodeEt indt (ElmList etd, n, _) =
            indtTxts indt $ [T.concat["(\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"]
                            ,T.concat["    decodeList l",T.pack $ show indt," <|"]
                            ,T.unlines $ decodeEt (indt+2) etd,")"
                            ]
        decodeEt indt (ElmType name, n, _) =
            indtTxts indt $ [T.concat["(\\(r",T.pack $ show (indt-1),",l",T.pack $ show indt,") ->"] 
                            ,T.concat["    decode",T.pack name," (r",T.pack $ show (indt-1),",l",T.pack $ show indt,")"]
                            ]
        cases = map (\(constrName,edt) -> T.concat [ T.pack "        (\"",T.pack constrName, T.pack "\"", " " .:. " rest) ->"
                                                   ,"\n            (Err \"\",rest) |> \n"
                                                   ,T.unlines $ concat $ map (\(n,et) -> (decodeEt (4+n) et) ++ indtTxts (4+n) [" |>"]) $ zip [0..] edt
                                                   ,indtTxt (5 + length edt) $ T.concat ["(\\(r"
                                                                                        ,T.pack $ show $ length edt + 3,",l",T.pack $ show $ length edt + 4,") -> (",if length edt > 1 then T.concat ["rMap",T.pack $ show $ length edt] else "Ok <|"," ",T.pack constrName," ",T.intercalate " " $ map (\n -> T.pack $ "r" ++ show (n+4)) [0..length edt-1],",l",T.pack $ show $ length edt + 4,"))"]
                                                   ]) edts
    in
        T.unlines [typeSig
                  ,T.concat["decode",T.pack name," (lastRes,",T.toLower $ T.pack name,"Txts) = "]
                  ,T.concat["    case ",T.toLower $ T.pack name, "Txts of"]
                  ,T.unlines cases
                  ,T.concat["        _ -> (Err <| \"Incorrect input, could not decode value of type ",T.pack name," from string \\\"\" ++ sConcat ", T.toLower $ T.pack name,"Txts ++ \"\\\"\",[])"]
                  ]