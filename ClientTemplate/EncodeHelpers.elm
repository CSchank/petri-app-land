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

decodeInt : String -> Int
decodeInt s =
    let 
        decodeInt_ n s_ = case s_ of
                            f::rest -> (toCode f - 48) * n + decodeInt_ (n*64) rest
                            []      -> 0
    in
        decodeInt_ 1 (toList s)