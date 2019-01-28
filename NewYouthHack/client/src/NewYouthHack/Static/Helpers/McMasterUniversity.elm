module NewYouthHack.Static.Helpers.McMasterUniversity exposing (..)
import Dict exposing (Dict)

import Static.Types exposing(..)
import Static.ExtraUserTypes exposing(..)
import NewYouthHack.Static.Types
import Static.List
getNotices : McMasterUniversity -> (List Notice)
getNotices (McMasterUniversity notices _)  = notices

getStatements : McMasterUniversity -> (List (Int, Int, String))
getStatements (McMasterUniversity _ statements)  = statements



updateNotices : (List Notice) -> McMasterUniversity -> McMasterUniversity
updateNotices newnotices (McMasterUniversity notices statements)  = (McMasterUniversity newnotices statements) 

updateStatements : (List (Int, Int, String)) -> McMasterUniversity -> McMasterUniversity
updateStatements newstatements (McMasterUniversity notices statements)  = (McMasterUniversity notices newstatements) 


alterNotices : ((List Notice) -> (List Notice)) -> McMasterUniversity -> McMasterUniversity
alterNotices f (McMasterUniversity notices statements)  = 
    let
        newnotices = f notices
    in
        (McMasterUniversity newnotices statements) 

alterStatements : ((List (Int, Int, String)) -> (List (Int, Int, String))) -> McMasterUniversity -> McMasterUniversity
alterStatements f (McMasterUniversity notices statements)  = 
    let
        newstatements = f statements
    in
        (McMasterUniversity notices newstatements) 



