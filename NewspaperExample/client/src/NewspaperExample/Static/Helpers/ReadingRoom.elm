module NewspaperExample.Static.Helpers.ReadingRoom exposing (..)
import Dict exposing (Dict)

import Static.Types exposing(..)
import Static.ExtraUserTypes exposing(..)
import NewspaperExample.Static.Types
import Static.List
getArticles : ReadingRoom -> (List Article)
getArticles (ReadingRoom articles _ _)  = articles

getTitles : ReadingRoom -> (List String)
getTitles (ReadingRoom _ titles _)  = titles

getMaybeViewing : ReadingRoom -> (Maybe String)
getMaybeViewing (ReadingRoom _ _ maybeViewing)  = maybeViewing



updateArticles : (List Article) -> ReadingRoom -> ReadingRoom
updateArticles newarticles (ReadingRoom articles titles maybeViewing)  = (ReadingRoom newarticles titles maybeViewing) 

updateTitles : (List String) -> ReadingRoom -> ReadingRoom
updateTitles newtitles (ReadingRoom articles titles maybeViewing)  = (ReadingRoom articles newtitles maybeViewing) 

updateMaybeViewing : (Maybe String) -> ReadingRoom -> ReadingRoom
updateMaybeViewing newmaybeViewing (ReadingRoom articles titles maybeViewing)  = (ReadingRoom articles titles newmaybeViewing) 


alterArticles : ((List Article) -> (List Article)) -> ReadingRoom -> ReadingRoom
alterArticles f (ReadingRoom articles titles maybeViewing)  = 
    let
        newarticles = f articles
    in
        (ReadingRoom newarticles titles maybeViewing) 

alterTitles : ((List String) -> (List String)) -> ReadingRoom -> ReadingRoom
alterTitles f (ReadingRoom articles titles maybeViewing)  = 
    let
        newtitles = f titles
    in
        (ReadingRoom articles newtitles maybeViewing) 

alterMaybeViewing : ((Maybe String) -> (Maybe String)) -> ReadingRoom -> ReadingRoom
alterMaybeViewing f (ReadingRoom articles titles maybeViewing)  = 
    let
        newmaybeViewing = f maybeViewing
    in
        (ReadingRoom articles titles newmaybeViewing) 



