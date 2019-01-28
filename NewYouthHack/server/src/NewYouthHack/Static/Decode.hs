{-# LANGUAGE OverloadedStrings #-}
module NewYouthHack.Static.Decode where
import NewYouthHack.Static.Types

import Utils.Utils
import qualified Data.Text as T
decodeTransition  ::  (Result T.Text Transition, [T.Text]) -> (Result T.Text Transition, [T.Text])
decodeTransition (lastRes,transitionTxts) = 
    case transitionTxts of
        ("TEnterMcMasterCreateNotice" : rest) ->
            (Err "",rest) |> 
                    (\(r3,l4) -> (Ok <| TEnterMcMasterCreateNotice ,l4))
        ("TCancelNotice" : rest) ->
            (Err "",rest) |> 
                    (\(r3,l4) -> (Ok <| TCancelNotice ,l4))
        ("TEnterUniversities" : rest) ->
            (Err "",rest) |> 
                    (\(r3,l4) -> (Ok <| TEnterUniversities ,l4))
        ("TExitUniversities" : rest) ->
            (Err "",rest) |> 
                    (\(r3,l4) -> (Ok <| TExitUniversities ,l4))
        ("TEnterMcMasterUniversity" : rest) ->
            (Err "",rest) |> 
                    (\(r3,l4) -> (Ok <| TEnterMcMasterUniversity ,l4))
        ("TExitMcMasterUniversity" : rest) ->
            (Err "",rest) |> 
                    (\(r3,l4) -> (Ok <| TExitMcMasterUniversity ,l4))
        ("TEditMcMasterComment" : rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeString l4
                 |>
                        (\(r4,l5) -> (rMap1 TEditMcMasterComment r4,l5))
        ("TSendMcMasterComment" : rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeString l4
                 |>
                        (\(r4,l5) -> (rMap1 TSendMcMasterComment r4,l5))
        ("TEditMcMasterNotice" : rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeString l4
                 |>
                        (\(r4,l5) -> (rMap1 TEditMcMasterNotice r4,l5))
        ("TPublishMcMasterNotice" : rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeNotice (r3,l4)
                 |>
                        (\(r4,l5) -> (rMap1 TPublishMcMasterNotice r4,l5))

        _ -> (Err <| tConcat ["Incorrect input, could not decode value of type Transition from string \"", tConcat transitionTxts, "\""],[])

-- extra type decoders
decodeNotice  ::  (Result T.Text Notice, [T.Text]) -> (Result T.Text Notice, [T.Text])
decodeNotice (lastRes,noticeTxts) = 
    case noticeTxts of
        ("Notice" : rest) ->
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
                                        (timestampTxt : ll7) -> (decodeInt 1548438211 9999999999 timestampTxt |> randThen Ok,ll7)
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
                                                        (uidCommentTxt : llf10) ->
                                                             ("",l10) |>
                                                                                                    \(r14,l15) ->
                                                                    (case l15 of
                                                                        (uidTxt : ll15) -> (decodeInt 0 99999 uidTxt |> randThen Ok,ll15)
                                                                        [] -> (Err "Ran out of string to process while parsing Notice",[]))

                                                        [] -> (Err "Ran out of string to process while parsing Notice",[])
                                                (comment,ls10) =
                                                    case lf10 of
                                                        (uidCommentTxt : lls10) ->
                                                             ("",lf10) |>
                                                                                                    \(r14,l15) ->
                                                                decodeString l15

                                                        [] -> (Err "Ran out of string to process while parsing Notice",[])
                                            in (rMap2 (\rff10 rss10 -> (rff10,rss10)) uid comment,ls10)) |>
                                        decodeList l9
                                     |>
                                            (\(r9,l10) -> (rMap6 Notice r4 r5 r6 r7 r8 r9,l10))

        _ -> (Err <| tConcat ["Incorrect input, could not decode value of type Notice from string \"", tConcat noticeTxts, "\""],[])


