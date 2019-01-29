module NewYouthHack.Static.Wrappers.McMasterCreateNotice exposing(..)
import NewYouthHack.Static.Types.McMasterCreateNotice exposing(..)
import NewYouthHack.Static.Types exposing(..)

unwrap : Msg -> OutgoingTransition
unwrap msg =
    case msg of
        NewYouthHack.Static.Types.McMasterCreateNotice.CancelNotice  -> TCancelNotice 
        (NewYouthHack.Static.Types.McMasterCreateNotice.EditMcMasterNotice partialNotice)  -> (TEditMcMasterNotice partialNotice) 
        (NewYouthHack.Static.Types.McMasterCreateNotice.PublishMcMasterNotice notice)  -> (TPublishMcMasterNotice notice) 

