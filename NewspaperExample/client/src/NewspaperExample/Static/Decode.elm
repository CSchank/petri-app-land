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

--extra types decoders
decodeArticle  :  (Result String Article, List String) -> (Result String Article, List String)
decodeArticle (lastRes,articleTxts) = 
    case articleTxts of
        ("Article" :: rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeString l4
                 |>
                    \(r4,l5) ->
                        decodeString l5
                     |>
                        \(r5,l6) ->
                                (case l6 of
                                    (timestampTxt :: ll6) -> (decodeInt 0 999999999 timestampTxt |> randThen Ok,ll6)
                                    [] -> (Err "Ran out of string to process while parsing Article",[]))
                         |>
                            \(r6,l7) ->
                                decodeString l7
                             |>
                                    (\(r7,l8) -> (rMap4 Article r4 r5 r6 r7,l8))
        ("Letter" :: rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeString l4
                 |>
                    \(r4,l5) ->
                        decodeString l5
                     |>
                        \(r5,l6) ->
                                (case l6 of
                                    (timestampTxt :: ll6) -> (decodeInt 0 999999999 timestampTxt |> randThen Ok,ll6)
                                    [] -> (Err "Ran out of string to process while parsing Article",[]))
                         |>
                            \(r6,l7) ->
                                decodeString l7
                             |>
                                    (\(r7,l8) -> (rMap4 Letter r4 r5 r6 r7,l8))

        _ -> (Err <| tConcat ["Incorrect input, could not decode value of type Article from string \"", tConcat articleTxts, "\""],[])

decodeDraft  :  (Result String Draft, List String) -> (Result String Draft, List String)
decodeDraft (lastRes,draftTxts) = 
    case draftTxts of
        ("DraftArticle" :: rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeString l4
                 |>
                    \(r4,l5) ->
                        decodeString l5
                     |>
                        \(r5,l6) ->
                                (case l6 of
                                    (timestampTxt :: ll6) -> (decodeInt 0 999999999 timestampTxt |> randThen Ok,ll6)
                                    [] -> (Err "Ran out of string to process while parsing Draft",[]))
                         |>
                            \(r6,l7) ->
                                decodeString l7
                             |>
                                \(r7,l8) ->
                                (\(r8,l9) ->
                                        let
                                            (uid,lf9) =
                                                case l9 of
                                                    (uidCommentTxt :: llf9) ->
                                                         ("",l9) |>
                                                                                            \(r13,l14) ->
                                                                (case l14 of
                                                                    (uidTxt :: ll14) -> (decodeInt 0 99999 uidTxt |> randThen Ok,ll14)
                                                                    [] -> (Err "Ran out of string to process while parsing Draft",[]))

                                                    [] -> (Err "Ran out of string to process while parsing Draft",[])
                                            (comment,ls9) =
                                                case lf9 of
                                                    (uidCommentTxt :: lls9) ->
                                                         ("",lf9) |>
                                                                                            \(r13,l14) ->
                                                            decodeString l14

                                                    [] -> (Err "Ran out of string to process while parsing Draft",[])
                                        in (rMap2 (\rff9 rss9 -> (rff9,rss9)) uid comment,ls9)) |>
                                    decodeList l8
                                 |>
                                        (\(r8,l9) -> (rMap5 DraftArticle r4 r5 r6 r7 r8,l9))
        ("DraftLetter" :: rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeString l4
                 |>
                    \(r4,l5) ->
                        decodeString l5
                     |>
                        \(r5,l6) ->
                                (case l6 of
                                    (timestampTxt :: ll6) -> (decodeInt 0 999999999 timestampTxt |> randThen Ok,ll6)
                                    [] -> (Err "Ran out of string to process while parsing Draft",[]))
                         |>
                            \(r6,l7) ->
                                decodeString l7
                             |>
                                \(r7,l8) ->
                                (\(r8,l9) ->
                                        let
                                            (uid,lf9) =
                                                case l9 of
                                                    (uidCommentTxt :: llf9) ->
                                                         ("",l9) |>
                                                                                            \(r13,l14) ->
                                                                (case l14 of
                                                                    (uidTxt :: ll14) -> (decodeInt 0 99999 uidTxt |> randThen Ok,ll14)
                                                                    [] -> (Err "Ran out of string to process while parsing Draft",[]))

                                                    [] -> (Err "Ran out of string to process while parsing Draft",[])
                                            (comment,ls9) =
                                                case lf9 of
                                                    (uidCommentTxt :: lls9) ->
                                                         ("",lf9) |>
                                                                                            \(r13,l14) ->
                                                            decodeString l14

                                                    [] -> (Err "Ran out of string to process while parsing Draft",[])
                                        in (rMap2 (\rff9 rss9 -> (rff9,rss9)) uid comment,ls9)) |>
                                    decodeList l8
                                 |>
                                        (\(r8,l9) -> (rMap5 DraftLetter r4 r5 r6 r7 r8,l9))

        _ -> (Err <| tConcat ["Incorrect input, could not decode value of type Draft from string \"", tConcat draftTxts, "\""],[])


