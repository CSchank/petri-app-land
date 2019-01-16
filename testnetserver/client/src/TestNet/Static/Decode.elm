module TestNet.Static.Decode exposing(..)
import TestNet.Static.Types

import Utils.Utils
decodeIncomingTransition  :  (Result String IncomingTransition, List String) -> (Result String IncomingTransition, List String)
decodeIncomingTransition (lastRes,incomingtransitionTxts) = 
    case incomingtransitionTxts of
        ("MStartGameCA" :: rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                        (case l4 of
                            (nTxt :: ll4) -> (decodeInt 0 1000 nTxt |> randThen Ok,ll4)
                            [] -> (Err "Ran out of string to process while parsing IncomingTransition",[]))
                 |>
                        (\(r4,l5) -> (rMap1 MStartGameCA r4,l5))
        ("MStartGameAB2" :: rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                        (case l4 of
                            (nTxt :: ll4) -> (decodeInt 0 1000 nTxt |> randThen Ok,ll4)
                            [] -> (Err "Ran out of string to process while parsing IncomingTransition",[]))
                 |>
                        (\(r4,l5) -> (rMap1 MStartGameAB2 r4,l5))
        ("MStartGameAC" :: rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                        (case l4 of
                            (nTxt :: ll4) -> (decodeInt 0 1000 nTxt |> randThen Ok,ll4)
                            [] -> (Err "Ran out of string to process while parsing IncomingTransition",[]))
                 |>
                        (\(r4,l5) -> (rMap1 MStartGameAC r4,l5))
        ("MStartGameBC" :: rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                        (case l4 of
                            (nTxt :: ll4) -> (decodeInt 0 1000 nTxt |> randThen Ok,ll4)
                            [] -> (Err "Ran out of string to process while parsing IncomingTransition",[]))
                 |>
                        (\(r4,l5) -> (rMap1 MStartGameBC r4,l5))

        _ -> (Err <| tConcat ["Incorrect input, could not decode value of type IncomingTransition from string \"", tConcat incomingtransitionTxts, "\""],[])

