module NewspaperExample.Static.Helpers.EditingRoom exposing (..)
import Dict exposing (Dict)

import Static.Types exposing(..)
import Static.ExtraUserTypes exposing(..)
import NewspaperExample.Static.Types
import Static.List
getMaybeEditing : EditingRoom -> (Maybe Draft)
getMaybeEditing (EditingRoom maybeEditing _)  = maybeEditing

getTitles : EditingRoom -> (List String)
getTitles (EditingRoom _ titles)  = titles



updateMaybeEditing : (Maybe Draft) -> EditingRoom -> EditingRoom
updateMaybeEditing newmaybeEditing (EditingRoom maybeEditing titles)  = (EditingRoom newmaybeEditing titles) 

updateTitles : (List String) -> EditingRoom -> EditingRoom
updateTitles newtitles (EditingRoom maybeEditing titles)  = (EditingRoom maybeEditing newtitles) 


alterMaybeEditing : ((Maybe Draft) -> (Maybe Draft)) -> EditingRoom -> EditingRoom
alterMaybeEditing f (EditingRoom maybeEditing titles)  = 
    let
        newmaybeEditing = f maybeEditing
    in
        (EditingRoom newmaybeEditing titles) 

alterTitles : ((List String) -> (List String)) -> EditingRoom -> EditingRoom
alterTitles f (EditingRoom maybeEditing titles)  = 
    let
        newtitles = f titles
    in
        (EditingRoom maybeEditing newtitles) 



