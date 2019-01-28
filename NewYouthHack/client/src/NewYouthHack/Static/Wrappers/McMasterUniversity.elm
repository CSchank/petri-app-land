module NewYouthHack.Static.Wrappers.McMasterUniversity exposing(..)
import NewYouthHack.Static.Types.McMasterUniversity exposing(..)
import NewYouthHack.Static.Types exposing(..)

unwrap : Msg -> OutgoingTransition
unwrap msg =
    case msg of
        NewYouthHack.Static.Types.McMasterUniversity.EnterMcMasterCreateNotice  -> TEnterMcMasterCreateNotice 
        NewYouthHack.Static.Types.McMasterUniversity.ExitMcMasterUniversity  -> TExitMcMasterUniversity 
        (NewYouthHack.Static.Types.McMasterUniversity.EditMcMasterComment partialComment)  -> (TEditMcMasterComment partialComment) 
        (NewYouthHack.Static.Types.McMasterUniversity.SendMcMasterComment comment)  -> (TSendMcMasterComment comment) 
        (NewYouthHack.Static.Types.McMasterUniversity.PublishMcMasterNotice notice)  -> (TPublishMcMasterNotice notice) 

