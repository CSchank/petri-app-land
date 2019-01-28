module Static.Init exposing (..)
import Static.Types exposing(..)
import NewYouthHack.Static.Init

init : (NetModel, Cmd NetOutgoingTransition)
init = (NewYouthHack NewYouthHack.Static.Init.init, Cmd.none)
