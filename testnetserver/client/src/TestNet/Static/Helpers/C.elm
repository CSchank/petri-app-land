module TestNet.Static.Helpers.C exposing (..)
import Dict exposing (Dict)

import Static.Types exposing(..)
import Static.ExtraUserTypes exposing(..)
import TestNet.Static.Types
import Static.List
getN : C -> Int
getN (C n _)  = n

getNLst : C -> (List Int)
getNLst (C _ nLst)  = nLst



updateN : Int -> C -> C
updateN newn (C n nLst)  = (C newn nLst) 

updateNLst : (List Int) -> C -> C
updateNLst newnLst (C n nLst)  = (C n newnLst) 


alterN : (Int -> Int) -> C -> C
alterN f (C n nLst)  = 
    let
        newn = f n
    in
        (C newn nLst) 

alterNLst : ((List Int) -> (List Int)) -> C -> C
alterNLst f (C n nLst)  = 
    let
        newnLst = f nLst
    in
        (C n newnLst) 



