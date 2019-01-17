module TestNet.Static.Init exposing(..)
import TestNet.Init as Init
import TestNet.Static.Types exposing (NetState(..))
import TestNet.Update as Update
import TestNet.Static.Wrappers
init : NetState
init = SA Init.init
