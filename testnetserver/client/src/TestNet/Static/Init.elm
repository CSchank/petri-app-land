module TestNet.Static.Init exposing(..)
import TestNet.Init as Init
import TestNet.Update as Update
import TestNet.Static.Wrappers
init :: NetState
init = SA Init.init
