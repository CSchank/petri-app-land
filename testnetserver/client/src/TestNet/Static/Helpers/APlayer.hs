module TestNet.Static.Helpers.APlayer where

import Data.Map as Dict

import Static.Types
import TestNet.Static.Types
import Static.List
getPlayerN :: APlayer -> Int
getPlayerN (APlayer playerN)  = playerN



updatePlayerN :: Int -> APlayer -> APlayer
updatePlayerN newplayerN (APlayer playerN)  = (APlayer newplayerN) 


alterPlayerN :: (Int -> Int) -> APlayer -> APlayer
alterPlayerN f (APlayer playerN)  = 
    let
        newplayerN = f playerN
    in
        (APlayer newplayerN) 



