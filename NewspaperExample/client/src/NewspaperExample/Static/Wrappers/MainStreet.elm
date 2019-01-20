module NewspaperExample.Static.Wrappers.MainStreet exposing(..)
import NewspaperExample.Static.Types.MainStreet exposing(..)
import NewspaperExample.Static.Types exposing(..)

unwrap : Msg -> OutgoingTransition
unwrap msg =
    case msg of
        NewspaperExample.Static.Types.MainStreet.EnterReadingRoom  -> TEnterReadingRoom 
        NewspaperExample.Static.Types.MainStreet.EnterEditingRoom  -> TEnterEditingRoom 

