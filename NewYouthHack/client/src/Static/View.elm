module Static.View exposing(..)
import NewYouthHack.Static.View as NewYouthHack

import Static.Types exposing(..)
import Html exposing(Html)
view : NetModel -> Html NetOutgoingTransition
view model =
    case model of
        NewYouthHack m -> Html.map NewYouthHackOTrans <| NewYouthHack.view m

title : NetModel -> String
title model =
    case model of
        NewYouthHack m -> NewYouthHack.title m

