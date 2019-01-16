module Static.Init exposing (..)
import Static.Types
import TestNet.Static.Init exposing(..)


init = TestNet.Static.Init.init
-- reference to the initial Net
initNet :: NetModel
initNet = TestNet
