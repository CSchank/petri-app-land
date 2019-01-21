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

-- extra type decoders
decodeArticle  ::  (Result T.Text Article, [T.Text]) -> (Result T.Text Article, [T.Text])
decodeArticle (lastRes,articleTxts) = 
    case articleTxts of
        ("Article" : rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeString l4
                 |>
                    \(r4,l5) ->
                        decodeString l5
                     |>
                        \(r5,l6) ->
                                (case l6 of
                                    (timestampTxt : ll6) -> (decodeInt 0 999999999 timestampTxt |> randThen Ok,ll6)
                                    [] -> (Err "Ran out of string to process while parsing Article",[]))
                         |>
                            \(r6,l7) ->
                                decodeString l7
                             |>
                                    (\(r7,l8) -> (rMap4 Article r4 r5 r6 r7,l8))
        ("Letter" : rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeString l4
                 |>
                    \(r4,l5) ->
                        decodeString l5
                     |>
                        \(r5,l6) ->
                                (case l6 of
                                    (timestampTxt : ll6) -> (decodeInt 0 999999999 timestampTxt |> randThen Ok,ll6)
                                    [] -> (Err "Ran out of string to process while parsing Article",[]))
                         |>
                            \(r6,l7) ->
                                decodeString l7
                             |>
                                    (\(r7,l8) -> (rMap4 Letter r4 r5 r6 r7,l8))

        _ -> (Err <| tConcat ["Incorrect input, could not decode value of type Article from string \"", tConcat articleTxts, "\""],[])

decodeDraft  ::  (Result T.Text Draft, [T.Text]) -> (Result T.Text Draft, [T.Text])
decodeDraft (lastRes,draftTxts) = 
    case draftTxts of
        ("DraftArticle" : rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeString l4
                 |>
                    \(r4,l5) ->
                        decodeString l5
                     |>
                        \(r5,l6) ->
                                (case l6 of
                                    (timestampTxt : ll6) -> (decodeInt 0 999999999 timestampTxt |> randThen Ok,ll6)
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
                                                    (uidCommentTxt : llf9) ->
                                                         ("",l9) |>
                                                                                            \(r13,l14) ->
                                                                (case l14 of
                                                                    (uidTxt : ll14) -> (decodeInt 0 99999 uidTxt |> randThen Ok,ll14)
                                                                    [] -> (Err "Ran out of string to process while parsing Draft",[]))

                                                    [] -> (Err "Ran out of string to process while parsing Draft",[])
                                            (comment,ls9) =
                                                case lf9 of
                                                    (uidCommentTxt : lls9) ->
                                                         ("",lf9) |>
                                                                                            \(r13,l14) ->
                                                            decodeString l14

                                                    [] -> (Err "Ran out of string to process while parsing Draft",[])
                                        in (rMap2 (\rff9 rss9 -> (rff9,rss9)) uid comment,ls9)) |>
                                    decodeList l8
                                 |>
                                        (\(r8,l9) -> (rMap5 DraftArticle r4 r5 r6 r7 r8,l9))
        ("DraftLetter" : rest) ->
            (Err "",rest) |> 
                \(r3,l4) ->
                    decodeString l4
                 |>
                    \(r4,l5) ->
                        decodeString l5
                     |>
                        \(r5,l6) ->
                                (case l6 of
                                    (timestampTxt : ll6) -> (decodeInt 0 999999999 timestampTxt |> randThen Ok,ll6)
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
                                                    (uidCommentTxt : llf9) ->
                                                         ("",l9) |>
                                                                                            \(r13,l14) ->
                                                                (case l14 of
                                                                    (uidTxt : ll14) -> (decodeInt 0 99999 uidTxt |> randThen Ok,ll14)
                                                                    [] -> (Err "Ran out of string to process while parsing Draft",[]))

                                                    [] -> (Err "Ran out of string to process while parsing Draft",[])
                                            (comment,ls9) =
                                                case lf9 of
                                                    (uidCommentTxt : lls9) ->
                                                         ("",lf9) |>
                                                                                            \(r13,l14) ->
                                                            decodeString l14

                                                    [] -> (Err "Ran out of string to process while parsing Draft",[])
                                        in (rMap2 (\rff9 rss9 -> (rff9,rss9)) uid comment,ls9)) |>
                                    decodeList l8
                                 |>
                                        (\(r8,l9) -> (rMap5 DraftLetter r4 r5 r6 r7 r8,l9))

        _ -> (Err <| tConcat ["Incorrect input, could not decode value of type Draft from string \"", tConcat draftTxts, "\""],[])


