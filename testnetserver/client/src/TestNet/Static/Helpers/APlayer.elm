module TestNet.Static.Helpers.APlayer exposing (..)
import Dict exposing (Dict)

import Static.Types exposing(..)
import Static.ExtraUserTypes exposing(..)
import TestNet.Static.Types
import Static.List
getPlayerN : APlayer -> Int
getPlayerN (APlayer playerN)  = playerN



updatePlayerN : Int -> APlayer -> APlayer
updatePlayerN newplayerN (APlayer playerN)  = (APlayer newplayerN) 


alterPlayerN : (Int -> Int) -> APlayer -> APlayer
alterPlayerN f (APlayer playerN)  = 
    let
        newplayerN = f playerN
    in
        (APlayer newplayerN) 



