module TestNet.Static.Helpers.CPlayer exposing (..)
import Dict exposing (Dict)

import Static.Types exposing(..)
import Static.ExtraUserTypes exposing(..)
import TestNet.Static.Types
import Static.List
getPlayerN : CPlayer -> Int
getPlayerN (CPlayer playerN)  = playerN



updatePlayerN : Int -> CPlayer -> CPlayer
updatePlayerN newplayerN (CPlayer playerN)  = (CPlayer newplayerN) 


alterPlayerN : (Int -> Int) -> CPlayer -> CPlayer
alterPlayerN f (CPlayer playerN)  = 
    let
        newplayerN = f playerN
    in
        (CPlayer newplayerN) 



