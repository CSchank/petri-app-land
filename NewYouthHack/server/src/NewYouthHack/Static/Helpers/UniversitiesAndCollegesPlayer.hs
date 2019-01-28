module NewYouthHack.Static.Helpers.UniversitiesAndCollegesPlayer where

import Data.Map as Dict

import Static.Types
import NewYouthHack.Static.Types
import Static.List
getClientID :: UniversitiesAndCollegesPlayer -> Int
getClientID (UniversitiesAndCollegesPlayer clientID)  = clientID



updateClientID :: Int -> UniversitiesAndCollegesPlayer -> UniversitiesAndCollegesPlayer
updateClientID newclientID (UniversitiesAndCollegesPlayer clientID)  = (UniversitiesAndCollegesPlayer newclientID) 


alterClientID :: (Int -> Int) -> UniversitiesAndCollegesPlayer -> UniversitiesAndCollegesPlayer
alterClientID f (UniversitiesAndCollegesPlayer clientID)  = 
    let
        newclientID = f clientID
    in
        (UniversitiesAndCollegesPlayer newclientID) 



