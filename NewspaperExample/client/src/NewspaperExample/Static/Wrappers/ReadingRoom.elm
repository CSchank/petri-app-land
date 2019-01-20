module NewspaperExample.Static.Wrappers.ReadingRoom exposing(..)
import NewspaperExample.Static.Types.ReadingRoom exposing(..)
import NewspaperExample.Static.Types exposing(..)

unwrap : Msg -> OutgoingTransition
unwrap msg =
    case msg of
        NewspaperExample.Static.Types.ReadingRoom.LeaveReadingRoom  -> TLeaveReadingRoom 

