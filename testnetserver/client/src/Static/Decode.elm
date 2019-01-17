module Static.Decode exposing(..)
import Static.Types exposing(..)
import Utils.Utils exposing(..)
import TestNet.Static.Decode


decodeIncomingMessage : String -> NetModel -> Result String NetIncomingMessage
decodeIncomingMessage txt clientNet =
    case clientNet of
        TestNet _ -> rMap TestNetInMsg <| Tuple.first <| TestNet.Static.Decode.decodeIncomingMessage (Err "",String.split "\u{0000}" txt)
