module TestNet.Static.Helpers.BPlayer where

import Data.Map as Dict

import Static.Types
import TestNet.Static.Types
import Static.List
getPlayerN :: BPlayer -> Int
getPlayerN (BPlayer playerN)  = playerN



updatePlayerN :: Int -> BPlayer -> BPlayer
updatePlayerN newplayerN (BPlayer playerN)  = (BPlayer newplayerN) 


alterPlayerN :: (Int -> Int) -> BPlayer -> BPlayer
alterPlayerN f (BPlayer playerN)  = 
    let
        newplayerN = f playerN
    in
        (BPlayer newplayerN) 



