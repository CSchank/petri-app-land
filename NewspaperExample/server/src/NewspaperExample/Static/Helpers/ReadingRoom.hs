module NewspaperExample.Static.Helpers.ReadingRoom where

import Data.Map as Dict

import Static.Types
import NewspaperExample.Static.Types
import Static.List
getArticles :: ReadingRoom -> (List Article)
getArticles (ReadingRoom articles)  = articles



updateArticles :: (List Article) -> ReadingRoom -> ReadingRoom
updateArticles newarticles (ReadingRoom articles)  = (ReadingRoom newarticles) 


alterArticles :: ((List Article) -> (List Article)) -> ReadingRoom -> ReadingRoom
alterArticles f (ReadingRoom articles)  = 
    let
        newarticles = f articles
    in
        (ReadingRoom newarticles) 



