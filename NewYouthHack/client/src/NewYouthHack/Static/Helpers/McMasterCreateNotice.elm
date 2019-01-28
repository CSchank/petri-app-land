module NewYouthHack.Static.Helpers.McMasterCreateNotice exposing (..)
import Dict exposing (Dict)

import Static.Types exposing(..)
import Static.ExtraUserTypes exposing(..)
import NewYouthHack.Static.Types
import Static.List
getNotice : McMasterCreateNotice -> Notice
getNotice (McMasterCreateNotice notice)  = notice



updateNotice : Notice -> McMasterCreateNotice -> McMasterCreateNotice
updateNotice newnotice (McMasterCreateNotice notice)  = (McMasterCreateNotice newnotice) 


alterNotice : (Notice -> Notice) -> McMasterCreateNotice -> McMasterCreateNotice
alterNotice f (McMasterCreateNotice notice)  = 
    let
        newnotice = f notice
    in
        (McMasterCreateNotice newnotice) 



