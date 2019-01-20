module NewspaperExample.Static.View exposing(..)
import Html exposing(Html)
import Static.Types exposing (NetModel)
import NewspaperExample.Static.Types exposing(..)
import NewspaperExample.View.MainStreet
import NewspaperExample.View.ReadingRoom
import NewspaperExample.View.EditingRoom

import NewspaperExample.Static.Wrappers.MainStreet
import NewspaperExample.Static.Wrappers.ReadingRoom
import NewspaperExample.Static.Wrappers.EditingRoom


view : NetState -> Html OutgoingTransition
view ns =
    case ns of
        SMainStreet m -> Html.map NewspaperExample.Static.Wrappers.MainStreet.unwrap <| NewspaperExample.View.MainStreet.view m
        SReadingRoom m -> Html.map NewspaperExample.Static.Wrappers.ReadingRoom.unwrap <| NewspaperExample.View.ReadingRoom.view m
        SEditingRoom m -> Html.map NewspaperExample.Static.Wrappers.EditingRoom.unwrap <| NewspaperExample.View.EditingRoom.view m

title : NetState -> String
title ns =
    case ns of
        SMainStreet m -> NewspaperExample.View.MainStreet.title m
        SReadingRoom m -> NewspaperExample.View.ReadingRoom.title m
        SEditingRoom m -> NewspaperExample.View.EditingRoom.title m

