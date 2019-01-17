module TestNet.Static.Wrappers.A exposing(..)
import TestNet.Static.Types.A exposing(..)
import TestNet.Static.Types exposing(..)

unwrap : Msg -> OutgoingTransition
unwrap msg =
    case msg of
        (TestNet.Static.Types.A.AB n)  -> (TCA n) 
        (TestNet.Static.Types.A.ABC n)  -> (TABC n) 

