module NewYouthHack.Static.Helpers.SquareOne where

import Data.Map as Dict

import Static.Types
import NewYouthHack.Static.Types
import Static.List
getNotices :: SquareOne -> (List Notice)
getNotices (SquareOne notices)  = notices



updateNotices :: (List Notice) -> SquareOne -> SquareOne
updateNotices newnotices (SquareOne notices)  = (SquareOne newnotices) 


alterNotices :: ((List Notice) -> (List Notice)) -> SquareOne -> SquareOne
alterNotices f (SquareOne notices)  = 
    let
        newnotices = f notices
    in
        (SquareOne newnotices) 



