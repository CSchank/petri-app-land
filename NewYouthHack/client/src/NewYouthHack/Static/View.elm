module NewYouthHack.Static.View exposing(..)
import Html exposing(Html)
import Static.Types exposing (NetModel)
import NewYouthHack.Static.Types exposing(..)
import NewYouthHack.View.SquareOne
import NewYouthHack.View.UniversitiesAndColleges
import NewYouthHack.View.McMasterUniversity
import NewYouthHack.View.McMasterCreateNotice

import NewYouthHack.Static.Wrappers.SquareOne
import NewYouthHack.Static.Wrappers.UniversitiesAndColleges
import NewYouthHack.Static.Wrappers.McMasterUniversity
import NewYouthHack.Static.Wrappers.McMasterCreateNotice


view : NetState -> Html OutgoingTransition
view ns =
    case ns of
        SSquareOne m -> Html.map NewYouthHack.Static.Wrappers.SquareOne.unwrap <| NewYouthHack.View.SquareOne.view m
        SUniversitiesAndColleges m -> Html.map NewYouthHack.Static.Wrappers.UniversitiesAndColleges.unwrap <| NewYouthHack.View.UniversitiesAndColleges.view m
        SMcMasterUniversity m -> Html.map NewYouthHack.Static.Wrappers.McMasterUniversity.unwrap <| NewYouthHack.View.McMasterUniversity.view m
        SMcMasterCreateNotice m -> Html.map NewYouthHack.Static.Wrappers.McMasterCreateNotice.unwrap <| NewYouthHack.View.McMasterCreateNotice.view m

title : NetState -> String
title ns =
    case ns of
        SSquareOne m -> NewYouthHack.View.SquareOne.title m
        SUniversitiesAndColleges m -> NewYouthHack.View.UniversitiesAndColleges.title m
        SMcMasterUniversity m -> NewYouthHack.View.McMasterUniversity.title m
        SMcMasterCreateNotice m -> NewYouthHack.View.McMasterCreateNotice.title m

