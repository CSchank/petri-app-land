module NewYouthHack.Static.Wrappers.SquareOne exposing(..)
import NewYouthHack.Static.Types.SquareOne exposing(..)
import NewYouthHack.Static.Types exposing(..)

unwrap : Msg -> OutgoingTransition
unwrap msg =
    case msg of
        NewYouthHack.Static.Types.SquareOne.EnterUniversities  -> TEnterUniversities 

