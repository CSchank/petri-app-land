{-# LANGUAGE OverloadedStrings #-}
module NewspaperExample.Static.Decode where
import NewspaperExample.Static.Types

import Utils.Utils
import qualified Data.Text as T
decodeTransition  ::  (Result T.Text Transition, [T.Text]) -> (Result T.Text Transition, [T.Text])
decodeTransition (lastRes,transitionTxts) = 
    case transitionTxts of
        ("TEnterReadingRoom" : rest) ->
            (Err "",rest) |> 
                    (\(r3,l4) -> (Ok <| TEnterReadingRoom ,l4))
        ("TEnterEditingRoom" : rest) ->
            (Err "",rest) |> 
                    (\(r3,l4) -> (Ok <| TEnterEditingRoom ,l4))
        ("TStartEditing" : rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeString l4
                 |>
                        (\(r4,l5) -> (rMap1 TStartEditing r4,l5))
        ("TLeaveReadingRoom" : rest) ->
            (Err "",rest) |> 
                    (\(r3,l4) -> (Ok <| TLeaveReadingRoom ,l4))
        ("TLeaveEditingRoom" : rest) ->
            (Err "",rest) |> 
                    (\(r3,l4) -> (Ok <| TLeaveEditingRoom ,l4))
        ("TPublishArticle" : rest) ->
            (Err "",rest) |> 
                    (\(r3,l4) -> (Ok <| TPublishArticle ,l4))
        ("TSaveDraft" : rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeDraft (r3,l4)
                 |>
                        (\(r4,l5) -> (rMap1 TSaveDraft r4,l5))
        ("TEnterTitle" : rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeString l4
                 |>
                        (\(r4,l5) -> (rMap1 TEnterTitle r4,l5))
        ("TEnterText" : rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeString l4
                 |>
                        (\(r4,l5) -> (rMap1 TEnterText r4,l5))
        ("TEnterComment" : rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeString l4
                 |>
                        (\(r4,l5) -> (rMap1 TEnterComment r4,l5))
        ("TPostComment" : rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeString l4
                 |>
                        (\(r4,l5) -> (rMap1 TPostComment r4,l5))

        _ -> (Err <| tConcat ["Incorrect input, could not decode value of type Transition from string \"", tConcat transitionTxts, "\""],[])

