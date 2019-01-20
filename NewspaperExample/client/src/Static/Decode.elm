module Static.Decode exposing(..)
import Static.Types exposing(..)
import Utils.Utils exposing(..)
import NewspaperExample.Static.Decode


decodeIncomingMessage : String -> NetModel -> Result String NetIncomingMessage
decodeIncomingMessage txt clientNet =
    case clientNet of
        NewspaperExample _ -> rMap NewspaperExampleInMsg <| Tuple.first <| NewspaperExample.Static.Decode.decodeIncomingMessage (Err "",String.split "\u{0000}" txt)
