module TestNet.Static.Helpers.A exposing (..)
import Dict exposing (Dict)

import Static.Types exposing(..)
import Static.ExtraUserTypes exposing(..)
import TestNet.Static.Types
import Static.List
getN : A -> Int
getN (A n _)  = n

getNLst : A -> (List Int)
getNLst (A _ nLst)  = nLst



updateN : Int -> A -> A
updateN newn (A n nLst)  = (A newn nLst) 

updateNLst : (List Int) -> A -> A
updateNLst newnLst (A n nLst)  = (A n newnLst) 


alterN : (Int -> Int) -> A -> A
alterN f (A n nLst)  = 
    let
        newn = f n
    in
        (A newn nLst) 

alterNLst : ((List Int) -> (List Int)) -> A -> A
alterNLst f (A n nLst)  = 
    let
        newnLst = f nLst
    in
        (A n newnLst) 



