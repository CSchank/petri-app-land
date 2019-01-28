module NewYouthHack.Static.Helpers.McMasterCreateNoticePlayer where

import Data.Map as Dict

import Static.Types
import NewYouthHack.Static.Types
import Static.List
getClientID :: McMasterCreateNoticePlayer -> Int
getClientID (McMasterCreateNoticePlayer clientID)  = clientID



updateClientID :: Int -> McMasterCreateNoticePlayer -> McMasterCreateNoticePlayer
updateClientID newclientID (McMasterCreateNoticePlayer clientID)  = (McMasterCreateNoticePlayer newclientID) 


alterClientID :: (Int -> Int) -> McMasterCreateNoticePlayer -> McMasterCreateNoticePlayer
alterClientID f (McMasterCreateNoticePlayer clientID)  = 
    let
        newclientID = f clientID
    in
        (McMasterCreateNoticePlayer newclientID) 



