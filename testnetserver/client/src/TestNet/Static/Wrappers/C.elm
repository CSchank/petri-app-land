module TestNet.Static.Wrappers.C exposing(..)
import TestNet.Static.Types.C exposing(..)
import TestNet.Static.Types exposing(..)

unwrap : Msg -> OutgoingTransition
unwrap msg =
    case msg of
        (TestNet.Static.Types.C.CA n)  -> (TCA n) 

