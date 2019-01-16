module Static.View exposing(..)
import TestNet.Static.View as TestNet

view : NetModel
view model =
    case model of
        TestNet m -> TestNet.view m

