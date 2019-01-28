module NewYouthHack.Static.Helpers.McMasterUniversityPlayer where

import Data.Map as Dict

import Static.Types
import NewYouthHack.Static.Types
import Static.List
getClientID :: McMasterUniversityPlayer -> Int
getClientID (McMasterUniversityPlayer clientID)  = clientID



updateClientID :: Int -> McMasterUniversityPlayer -> McMasterUniversityPlayer
updateClientID newclientID (McMasterUniversityPlayer clientID)  = (McMasterUniversityPlayer newclientID) 


alterClientID :: (Int -> Int) -> McMasterUniversityPlayer -> McMasterUniversityPlayer
alterClientID f (McMasterUniversityPlayer clientID)  = 
    let
        newclientID = f clientID
    in
        (McMasterUniversityPlayer newclientID) 



