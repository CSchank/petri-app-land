module NewspaperExample.Static.Helpers.EditingRoom where

import Data.Map as Dict

import Static.Types
import NewspaperExample.Static.Types
import Static.List
getArticles :: EditingRoom -> (List Draft)
getArticles (EditingRoom articles)  = articles



updateArticles :: (List Draft) -> EditingRoom -> EditingRoom
updateArticles newarticles (EditingRoom articles)  = (EditingRoom newarticles) 


alterArticles :: ((List Draft) -> (List Draft)) -> EditingRoom -> EditingRoom
alterArticles f (EditingRoom articles)  = 
    let
        newarticles = f articles
    in
        (EditingRoom newarticles) 



