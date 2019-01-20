module Static.Init exposing (..)
import Static.Types exposing(..)
import NewspaperExample.Static.Init

init : (NetModel, Cmd NetOutgoingTransition)
init = (NewspaperExample NewspaperExample.Static.Init.init, Cmd.none)
