module Static.View exposing(..)
import TestNet.Static.View as TestNet

import Static.Types exposing(..)
import Html exposing(Html)
view : NetModel -> Html NetOutgoingTransition
view model =
    case model of
        TestNet m -> Html.map TestNetOTrans <| TestNet.view m

title : NetModel -> String
title model =
    case model of
        TestNet m -> TestNet.title m

