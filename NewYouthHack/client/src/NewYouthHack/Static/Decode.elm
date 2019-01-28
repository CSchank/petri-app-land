module NewYouthHack.Static.Decode exposing(..)
import NewYouthHack.Static.Types exposing(..)

import Utils.Utils exposing(..)
decodeIncomingMessage  :  (Result String IncomingMessage, List String) -> (Result String IncomingMessage, List String)
decodeIncomingMessage (lastRes,incomingmessageTxts) = 
    case incomingmessageTxts of
        ("MDidEnterMcMasterCreateNotice" :: rest) ->
            (Err "",rest) |> 
                    (\(r3,l4) -> (Ok <| MDidEnterMcMasterCreateNotice ,l4))
        ("MDidCancelNotice" :: rest) ->
            (Err "",rest) |> 
                    (\(r3,l4) -> (Ok <| MDidCancelNotice ,l4))
        ("MDidEnterUniversitiesAndColleges" :: rest) ->
            (Err "",rest) |> 
                    (\(r3,l4) -> (Ok <| MDidEnterUniversitiesAndColleges ,l4))
        ("MDidLeaveUniversities" :: rest) ->
            (Err "",rest) |> 
                    (\(r3,l4) -> (Ok <| MDidLeaveUniversities ,l4))
        ("MDidEnterMcMasterUniversity" :: rest) ->
            (Err "",rest) |> 
                    (\(r3,l4) -> (Ok <| MDidEnterMcMasterUniversity ,l4))
        ("MDidLeaveMcMasterUniversity" :: rest) ->
            (Err "",rest) |> 
                    (\(r3,l4) -> (Ok <| MDidLeaveMcMasterUniversity ,l4))
        ("MDidEditMcMasterComment" :: rest) ->
            (Err "",rest) |> 
                    (\(r3,l4) -> (Ok <| MDidEditMcMasterComment ,l4))
        ("MDidSendMcMasterComment" :: rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeString l4
                 |>
                        (\(r4,l5) -> (rMap1 MDidSendMcMasterComment r4,l5))
        ("MDidEditMcMasterNotice" :: rest) ->
            (Err "",rest) |> 
                    (\(r3,l4) -> (Ok <| MDidEditMcMasterNotice ,l4))
        ("MDidPublishMcMasterNotice" :: rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeNotice (r3,l4)
                 |>
                        (\(r4,l5) -> (rMap1 MDidPublishMcMasterNotice r4,l5))
        ("MNewMcMasterNotice" :: rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeNotice (r3,l4)
                 |>
                        (\(r4,l5) -> (rMap1 MNewMcMasterNotice r4,l5))

        _ -> (Err <| tConcat ["Incorrect input, could not decode value of type IncomingMessage from string \"", tConcat incomingmessageTxts, "\""],[])

--extra types decoders
decodeNotice  :  (Result String Notice, List String) -> (Result String Notice, List String)
decodeNotice (lastRes,noticeTxts) = 
    case noticeTxts of
        ("Notice" :: rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeString l4
                 |>
                    \(r4,l5) ->
                        decodeString l5
                     |>
                        \(r5,l6) ->
                            decodeString l6
                         |>
                            \(r6,l7) ->
                                    (case l7 of
                                        (timestampTxt :: ll7) -> (decodeInt 1548438211 9999999999 timestampTxt |> randThen Ok,ll7)
                                        [] -> (Err "Ran out of string to process while parsing Notice",[]))
                             |>
                                \(r7,l8) ->
                                    decodeString l8
                                 |>
                                    \(r8,l9) ->
                                    (\(r9,l10) ->
                                            let
                                                (uid,lf10) =
                                                    case l10 of
                                                        (uidCommentTxt :: llf10) ->
                                                             ("",l10) |>
                                                                                                    \(r14,l15) ->
                                                                    (case l15 of
                                                                        (uidTxt :: ll15) -> (decodeInt 0 99999 uidTxt |> randThen Ok,ll15)
                                                                        [] -> (Err "Ran out of string to process while parsing Notice",[]))

                                                        [] -> (Err "Ran out of string to process while parsing Notice",[])
                                                (comment,ls10) =
                                                    case lf10 of
                                                        (uidCommentTxt :: lls10) ->
                                                             ("",lf10) |>
                                                                                                    \(r14,l15) ->
                                                                decodeString l15

                                                        [] -> (Err "Ran out of string to process while parsing Notice",[])
                                            in (rMap2 (\rff10 rss10 -> (rff10,rss10)) uid comment,ls10)) |>
                                        decodeList l9
                                     |>
                                            (\(r9,l10) -> (rMap6 Notice r4 r5 r6 r7 r8 r9,l10))

        _ -> (Err <| tConcat ["Incorrect input, could not decode value of type Notice from string \"", tConcat noticeTxts, "\""],[])


