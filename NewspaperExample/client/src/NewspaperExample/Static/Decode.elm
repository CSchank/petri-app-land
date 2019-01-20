module NewspaperExample.Static.Decode exposing(..)
import NewspaperExample.Static.Types exposing(..)

import Utils.Utils exposing(..)
decodeIncomingMessage  :  (Result String IncomingMessage, List String) -> (Result String IncomingMessage, List String)
decodeIncomingMessage (lastRes,incomingmessageTxts) = 
    case incomingmessageTxts of
        ("MDidEnterReadingRoom" :: rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                (\(r4,l5) ->
                        decodeArticle (r4,l5)) |>
                    decodeList l4
                 |>
                        (\(r4,l5) -> (rMap1 MDidEnterReadingRoom r4,l5))
        ("MDidEnterEditingRoom" :: rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                (\(r4,l5) ->
                        decodeString l5) |>
                    decodeList l4
                 |>
                        (\(r4,l5) -> (rMap1 MDidEnterEditingRoom r4,l5))
        ("MDidStartEditing" :: rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeDraft (r3,l4)
                 |>
                        (\(r4,l5) -> (rMap1 MDidStartEditing r4,l5))
        ("MDidLeaveReadingRoom" :: rest) ->
            (Err "",rest) |> 
                    (\(r3,l4) -> (Ok <| MDidLeaveReadingRoom ,l4))
        ("MDidLeaveEditingRoom" :: rest) ->
            (Err "",rest) |> 
                    (\(r3,l4) -> (Ok <| MDidLeaveEditingRoom ,l4))
        ("MDidPublish" :: rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                (\(r4,l5) ->
                        decodeArticle (r4,l5)) |>
                    decodeList l4
                 |>
                        (\(r4,l5) -> (rMap1 MDidPublish r4,l5))
        ("MDidSaveDraft" :: rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                (\(r4,l5) ->
                        decodeString l5) |>
                    decodeList l4
                 |>
                        (\(r4,l5) -> (rMap1 MDidSaveDraft r4,l5))
        ("MDidEnterTitle" :: rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                (\(r4,l5) ->
                        decodeString l5) |>
                    decodeList l4
                 |>
                        (\(r4,l5) -> (rMap1 MDidEnterTitle r4,l5))
        ("MDidEnterText" :: rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                (\(r4,l5) ->
                        decodeString l5) |>
                    decodeList l4
                 |>
                        (\(r4,l5) -> (rMap1 MDidEnterText r4,l5))
        ("MDidEnterComment" :: rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeString l4
                 |>
                        (\(r4,l5) -> (rMap1 MDidEnterComment r4,l5))
        ("MDidPostComment" :: rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeString l4
                 |>
                        (\(r4,l5) -> (rMap1 MDidPostComment r4,l5))

        _ -> (Err <| tConcat ["Incorrect input, could not decode value of type IncomingMessage from string \"", tConcat incomingmessageTxts, "\""],[])

