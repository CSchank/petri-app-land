module Static.View exposing(..)
import NewspaperExample.Static.View as NewspaperExample

import Static.Types exposing(..)
import Html exposing(Html)
view : NetModel -> Html NetOutgoingTransition
view model =
    case model of
        NewspaperExample m -> Html.map NewspaperExampleOTrans <| NewspaperExample.view m

title : NetModel -> String
title model =
    case model of
        NewspaperExample m -> NewspaperExample.title m

