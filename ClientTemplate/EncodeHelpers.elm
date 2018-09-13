module EncodeHelpers exposing(..)

import Char     exposing    (toCode, fromCode)
import String   exposing    (toList)
import Html     exposing    (text)

encodeInt : Int -> String
encodeInt n =
    let 
        b = 64
        r = modBy b n
        m = n // b
    in    
        if n == 0 then ""
        else (String.fromChar <| fromCode <| r + 48) ++ encodeInt m

decodeInt : Int -> Int -> String -> Result String Int
decodeInt low high s =
    let 
        decodeInt_ m s_ = case s_ of
                            f::rest -> (toCode f - 48) * m + decodeInt_ (m*64) rest
                            []      -> 0
        n = decodeInt_ 1 <| toList s
    in
        if n >= low && n <= high then  Ok <| n
        else                            Err <| "Could not decode " ++ String.fromInt n ++ " as it is outside the range [" ++ String.fromInt low ++ "," ++ String.fromInt high ++ "]."
