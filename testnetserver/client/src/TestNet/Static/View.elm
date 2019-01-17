module TestNet.Static.View exposing(..)
import Html exposing(Html)
import Static.Types exposing (NetModel)
import TestNet.Static.Types exposing(..)
import TestNet.View.A
import TestNet.View.B
import TestNet.View.C

import TestNet.Static.Wrappers.A
import TestNet.Static.Wrappers.B
import TestNet.Static.Wrappers.C


view : NetState -> Html OutgoingTransition
view ns =
    case ns of
        SA m -> Html.map TestNet.Static.Wrappers.A.unwrap <| TestNet.View.A.view m
        SB m -> Html.map TestNet.Static.Wrappers.B.unwrap <| TestNet.View.B.view m
        SC m -> Html.map TestNet.Static.Wrappers.C.unwrap <| TestNet.View.C.view m

