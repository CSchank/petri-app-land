module EncodeHelpers exposing(..)

import Char     exposing    (toCode, fromCode)
import String   exposing    (toList)
import Html     exposing    (text)
import Tuple    exposing    (second)
import Result

encodeInt : Int -> Int -> Int -> String
encodeInt low high n =
    let
        encodeInt_ :  Int -> String
        encodeInt_ nn =
            let 
                b = 64
                r = modBy b nn
                m = nn // b
            in    
                if nn == 0 then ""
                else (String.fromChar <| fromCode <| r + 48) ++ encodeInt_ m
    in
        encodeInt_ (clamp low high n)

encodeMaybe : Maybe a -> (a -> String) -> String
encodeMaybe m f =
    case m of
        Just m0 -> "J\u{0000}"++f m0
        Nothing -> "N"

decodeMaybe : List String -> ((Result String a, List String) -> (Result String a, List String)) -> (Result String (Maybe a), List String)
decodeMaybe ls decodeFn =
    case ls of
        ("J"::rest) -> 
            let
                (newRes, newLs) = decodeFn (Err "", mainLs)
            in
                (Result.map Just newRes,newLs)
        ("N"::rest) ->
            (Ok Nothing, rest)


decodeInt : Int -> Int -> String -> Result String Int
decodeInt low high s =
    let 
        decodeInt_ m s_ = case s_ of
                            f::rest -> (toCode f - 48) * m + decodeInt_ (m*64) rest
                            []      -> 0
        n = decodeInt_ 1 <| toList s
    in
        if n >= low && n <= high then  Ok <| n
        else                           Err <| "Could not decode " ++ String.fromInt n ++ " as it is outside the range [" ++ String.fromInt low ++ "," ++ String.fromInt high ++ "]."

decodeList : List String -> ((Result String a, List String) -> (Result String a, List String)) -> (Result String (List a), List String)
decodeList ls decodeFn =
    let 
        aR : Result String a -> Result String (List a) -> Result String (List a)
        aR aRes laRes =
            Result.map2 (\a la -> la ++ [a]) aRes laRes
        n =
            Result.withDefault 0 <| case ls of 
                nTxt :: rest -> decodeInt 0 16777215 nTxt
                []           -> Err "Could not decode number of items in list."

        decodeList_ n_ (resL, mainLs) = 
            let 
                (newRes, newLs) = decodeFn (Err "", mainLs)
            in
                (aR newRes resL, newLs)
    in
        List.foldl decodeList_ ([], ls) (List.range 1 n)