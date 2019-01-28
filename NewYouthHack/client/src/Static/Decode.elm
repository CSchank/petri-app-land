module Static.Decode exposing(..)
import Static.Types exposing(..)
import Utils.Utils exposing(..)
import NewYouthHack.Static.Decode


decodeIncomingMessage : String -> NetModel -> Result String NetIncomingMessage
decodeIncomingMessage txt clientNet =
    case clientNet of
        NewYouthHack _ -> rMap NewYouthHackInMsg <| Tuple.first <| NewYouthHack.Static.Decode.decodeIncomingMessage (Err "",String.split "\u{0000}" txt)
