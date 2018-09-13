{-# LANGUAGE OverloadedStrings #-}

module Generate.Codec where

import Types (ElmDocType(..), ElmType(..), ElmCustom(..), ClientStateDiagram, ServerStateDiagram, ClientServerApp, ClientTransition, ServerTransition)
import qualified Data.Map as M
import qualified Data.Text as T

--generateCodec :: ClientServerApp -> 

elmDelim = T.pack "\\u{0000}"

generateClientEncoders :: ElmCustom -> T.Text
generateClientEncoders (ElmCustom name edts) =
    let
        indtTxts indt txts = map (indtTxt indt) txts
        indtTxt indt txt = T.concat [T.replicate (4*indt) " ", txt]

        encodeEt :: Int -> ElmDocType -> [T.Text]
        encodeEt indt (ElmIntRange low high,n,_) = 
            indtTxts indt $ [T.concat[T.pack n, "Txt ="]
                            ,"    let"
                            ,T.concat["        low  = ", T.pack $ show low]
                            ,T.concat["        high = ", T.pack $ show high]
                            ,"    in" 
                            ,T.concat["        encodeInt (",T.pack n,"-low)"]
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
            indtTxts indt $ [T.concat[T.pack n, "Txt ="]
                            ,"    let"
                            ,T.concat["        (",T.pack n0,",",T.pack n1,") = ",T.pack n]
                            ,T.unlines $ encodeEt (indt+2) (et0,n0,d0)
                            ,T.unlines $ encodeEt (indt+2) (et1,n1,d1)
                            ,"    in"
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
            indtTxts indt $ [T.concat[T.pack n, "Txt ="]
                            ,"    let"
                            ,T.concat["        encode",T.pack n," ",T.pack n,"List ="]
                            ,T.concat["            case ",T.pack n,"List of"]
                            ,T.concat["                h::rest ->",T.pack n]
                            ,"                let"
                            ,T.unlines $ encodeEt (indt+5) (et, etn, etd)
                            ,"                in"
                            ,T.concat["                    ",T.pack etn,"Txt ++ encode",T.pack n," rest"]
                            ,"    in"
                            ,T.concat ["encode",T.pack n," ",T.pack n,"Txt"]
                            ]
        encodeEt indt (ElmType name, n, _) =
            indtTxts indt $ [T.concat[T.pack n,"Txt = encode",T.pack name," ",T.pack n]
                            ]
        cases = map (\(constrName,edt) -> T.concat ["        ",T.pack constrName,T.concat $ map (\(et,name,desc) -> T.pack $ " " ++ name) edt," -> "
                                                    ,"\n            let\n"
                                                    ,T.unlines $ concat $ map (encodeEt 4) edt
                                                    ,"            in\n"
                                                    ,"                \"",T.pack constrName,elmDelim,"\"++",T.intercalate (T.concat["++\"",elmDelim,"\"++"]) $ map (\(et,name,desc) -> T.pack $ name ++ "Txt") edt
                                                    ]) edts
    in
        T.unlines [T.concat["encode",T.pack name," : ",T.pack name," -> String"]
                  ,T.concat["encode",T.pack name," ",T.toLower $ T.pack name," = "]
                  ,T.concat["    case ",T.toLower $ T.pack name, " of"]
                  ,T.unlines cases
                  ]

generateClientDecoders :: ElmCustom -> T.Text
generateClientDecoders (ElmCustom name edts) =
    let
        indtTxts indt txts = map (indtTxt indt) txts
        indtTxt indt txt = T.concat [T.replicate (4*indt) " ", txt]

        decodeEt :: Int -> ElmDocType -> [T.Text]
        decodeEt indt (ElmIntRange low high,n,_) = 
            indtTxts indt $ [T.concat[T.pack n, " ="]
                            ,"    let"
                            ,T.concat["        low  = ", T.pack $ show low]
                            ,T.concat["        high = ", T.pack $ show high]
                            ,"    in" 
                            ,T.concat["        decodeInt low high ",T.pack n,"Txt |> Result.andThen (\\",T.pack n,"Res -> Ok <| ",T.pack n,"Res + low)"]
                            ]
        decodeEt indt (ElmFloatRange low high precision,n,_) = 
            indtTxts indt $ [T.concat[T.pack $ n," ="]
                            ,"    let"
                            ,T.concat["        low  = ", T.pack $ show $ round $ low * 10 ^ precision]
                            ,T.concat["        high = ", T.pack $ show $ round $ high * 10 ^ precision]
                            ,"    in" 
                            ,T.concat["        decodeInt low high ",T.pack n,"Txt |> Result.andThen (\\",T.pack n,"Res -> Ok <| toFloat ",T.pack n,"Res / ",T.pack $ show $ 10^precision," + low)"]
                            ]
        decodeEt indt (ElmString, n, _) =
            indtTxts indt $ [T.concat [T.pack n, " = ",T.pack n,"Txt"]]
        decodeEt indt (ElmSizedString size, n, _) =
            error "Not implemented yet"--indtTxts indt $ [T.concat[T.pack n, "Txt =",T.pack n]
        decodeEt indt (ElmPair (et0,n0,d0) (et1,n1,d1), n, _) =
            indtTxts indt $ [T.concat[T.pack n, " ="]
                            ,"    let"
                            ,T.concat["        (",T.pack n0,"Txt,",T.pack n1,"Txt) = ",T.pack n,"Txt"]
                            ,T.unlines $ decodeEt (indt+2) (et0,n0,d0)
                            ,T.unlines $ decodeEt (indt+2) (et1,n1,d1)
                            ,"    in"
                            ,T.concat ["        (",T.pack n0,",",T.pack n1,")"]
                            ]
        decodeEt indt (ElmTriple (et0,n0,d0) (et1,n1,d1) (et2,n2,d2), n, _) =
            indtTxts indt $ [T.concat[T.pack n, " ="]
                            ,"    let"
                            ,T.concat["        (",T.pack n0,"Txt,",T.pack n1,"Txt,",T.pack n2,"Txt) = ",T.pack n,"Txt"]
                            ,T.unlines $ decodeEt (indt+2) (et0,n0,d0)
                            ,T.unlines $ decodeEt (indt+2) (et1,n1,d1)
                            ,T.unlines $ decodeEt (indt+2) (et2,n2,d2)
                            ,"    in"
                            ,T.concat ["       (",T.pack n0,",",T.pack n1,",",T.pack n2,")"]
                            ]
        decodeEt indt (ElmList (et, etn, etd), n, _) =
            indtTxts indt $ [
                            ]
        decodeEt indt (ElmType name, n, _) =
            indtTxts indt $ [T.concat[T.pack n," = decode",T.pack name," ",T.pack n,"Txt"]
                            ]
        cases = map (\(constrName,edt) -> T.concat [ T.pack "        (\"",T.pack constrName, T.pack "\""
                                                   , T.concat $ map (\(et,name,desc) -> T.pack $ "::" ++ name ++ "Txt") edt
                                                   , T.pack "::rest) ->"
                                                   ,"\n            let\n"
                                                   ,T.unlines $ concat $ map (decodeEt 4) edt
                                                   ,"            in\n"
                                                   ,"                Result.map",if length edt > 1 then T.pack $ show $ length edt else ""," ",T.pack constrName," ",T.intercalate " " $ map (\(et,name,desc) -> T.pack $ name) edt
                                                   ]) edts
    in
        T.unlines [T.concat["decode",T.pack name," : ","String -> Result String ",T.pack name]
                  ,T.concat["decode",T.pack name," ",T.toLower $ T.pack name,"Txt = "]
                  ,T.concat["    case (String.split \"",elmDelim,"\" ",T.toLower $ T.pack name, "Txt) of"]
                  ,T.unlines cases
                  ,T.concat["        _ -> Err <| \"Incorrect input, could not decode value of type ",T.pack name," from string \\\"\" ++ ", T.toLower $ T.pack name,"Txt ++ \"\\\"\""]
                  ]