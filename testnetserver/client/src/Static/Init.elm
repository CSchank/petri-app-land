module Static.Init exposing (..)
import Static.Types exposing(..)
import TestNet.Static.Init

init : (NetModel, Cmd NetOutgoingTransition)
init = (TestNet TestNet.Static.Init.init, Cmd.none)
