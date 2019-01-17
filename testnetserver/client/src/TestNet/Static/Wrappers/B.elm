module TestNet.Static.Wrappers.B exposing(..)
import TestNet.Static.Types.B exposing(..)
import TestNet.Static.Types exposing(..)

unwrap : Msg -> OutgoingTransition
unwrap msg =
    case msg of
        (TestNet.Static.Types.B.ABC n)  -> (TCA n) 

