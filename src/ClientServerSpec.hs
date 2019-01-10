{-# LANGUAGE OverloadedStrings #-}

module ClientServerSpec where

import Types
import Data.Map as M
import TypeHelpers

outputDirectory = "testnetserver"

testNet :: Net
testNet =
    let
        place1 = 
            HybridPlace "A" 
                [edt (ElmIntRange 0 1000) "n" "",edt (ElmList $ edt (ElmIntRange 0 1000) "n" "") "nLst" ""] --server state
                [edt (ElmIntRange 0 1000) "playerN" ""] --player state
                [edt (ElmIntRange 0 1000) "n" ""]
                Nothing
                (Nothing, Nothing)
                Nothing

        place2 = 
            HybridPlace "B" 
                [edt (ElmIntRange 0 1000) "n" "",edt (ElmList $ edt (ElmIntRange 0 1000) "n" "") "nLst" ""] --server state
                [edt (ElmIntRange 0 1000) "playerN" ""] --player state
                [edt (ElmIntRange 0 1000) "n" ""]
                Nothing
                (Nothing, Nothing)
                Nothing

        place3 = 
            HybridPlace "C" 
                [edt (ElmIntRange 0 1000) "n" "",edt (ElmList $ edt (ElmIntRange 0 1000) "n" "") "nLst" ""] --server state
                [edt (ElmIntRange 0 1000) "playerN" ""] --player state
                [edt (ElmIntRange 0 1000) "n" ""]
                Nothing
                (Nothing, Nothing)
                Nothing
        trans1 =
            NetTransition
                (constructor "AB" [edt (ElmIntRange 0 1000) "n" ""])
                [("A", ("B", Just $ constructor "StartGameAB" [edt (ElmIntRange 0 1000) "n" ""]))]
                Nothing
        trans2 =
            NetTransition
                (constructor "CA" [edt (ElmIntRange 0 1000) "n" ""])
                [("C", ("A", Just $ constructor "StartGameCA" [edt (ElmIntRange 0 1000) "n" ""]))]
                Nothing
        trans3 =
            NetTransition
                (constructor "ABC" [edt (ElmIntRange 0 1000) "n" ""])
                [("A", ("B", Just $ constructor "StartGameAB2" [edt (ElmIntRange 0 1000) "n" ""]))
                ,("A", ("C", Just $ constructor "StartGameAC" [edt (ElmIntRange 0 1000) "n" ""]))
                ,("B", ("C", Just $ constructor "StartGameAC" [edt (ElmIntRange 0 1000) "n" ""]))
                ]
                Nothing
    in
        HybridNet
            "TestNet"
            "A"
            [place1,place2,place3]
            [(ServerOnlyTransition,trans1),(ClientOnlyTransition,trans2),(HybridTransition,trans3)]
            []

clientServerApp :: ClientServerApp
clientServerApp =
    ( "TestNet"           --starting net for a client
    , [testNet]           --all the nets in this client/server app
    , []                  --extra client types used in states or messages
    , []                  --extra server types used in states or messages
    )