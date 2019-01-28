module NewYouthHack.Static.Helpers.UniversitiesAndColleges where

import Data.Map as Dict

import Static.Types
import NewYouthHack.Static.Types
import Static.List
getNotices :: UniversitiesAndColleges -> (List Notice)
getNotices (UniversitiesAndColleges notices)  = notices



updateNotices :: (List Notice) -> UniversitiesAndColleges -> UniversitiesAndColleges
updateNotices newnotices (UniversitiesAndColleges notices)  = (UniversitiesAndColleges newnotices) 


alterNotices :: ((List Notice) -> (List Notice)) -> UniversitiesAndColleges -> UniversitiesAndColleges
alterNotices f (UniversitiesAndColleges notices)  = 
    let
        newnotices = f notices
    in
        (UniversitiesAndColleges newnotices) 



