module NewYouthHack.Static.Wrappers exposing(..)
import NewYouthHack.Static.Types exposing(..)

wrapDidEnterMcMasterCreateNotice : IncomingMessage -> DidEnterMcMasterCreateNotice
wrapDidEnterMcMasterCreateNotice x__ =
    case x__ of
        MDidEnterMcMasterCreateNotice  -> DidEnterMcMasterCreateNotice 
        _ -> DidEnterMcMasterCreateNotice 


wrapDidCancelNotice : IncomingMessage -> DidCancelNotice
wrapDidCancelNotice x__ =
    case x__ of
        MDidCancelNotice  -> DidCancelNotice 
        _ -> DidCancelNotice 


wrapDidEnterUniversitiesAndColleges : IncomingMessage -> DidEnterUniversitiesAndColleges
wrapDidEnterUniversitiesAndColleges x__ =
    case x__ of
        MDidEnterUniversitiesAndColleges  -> DidEnterUniversitiesAndColleges 
        _ -> DidEnterUniversitiesAndColleges 


wrapDidLeaveUniversities : IncomingMessage -> DidLeaveUniversities
wrapDidLeaveUniversities x__ =
    case x__ of
        MDidLeaveUniversities  -> DidLeaveUniversities 
        _ -> DidLeaveUniversities 


wrapDidEnterMcMasterUniversity : IncomingMessage -> DidEnterMcMasterUniversity
wrapDidEnterMcMasterUniversity x__ =
    case x__ of
        MDidEnterMcMasterUniversity  -> DidEnterMcMasterUniversity 
        _ -> DidEnterMcMasterUniversity 


wrapDidLeaveMcMasterUniversity : IncomingMessage -> DidLeaveMcMasterUniversity
wrapDidLeaveMcMasterUniversity x__ =
    case x__ of
        MDidLeaveMcMasterUniversity  -> DidLeaveMcMasterUniversity 
        _ -> DidLeaveMcMasterUniversity 


wrapDidEditMcMasterComment : IncomingMessage -> DidEditMcMasterComment
wrapDidEditMcMasterComment x__ =
    case x__ of
        MDidEditMcMasterComment  -> DidEditMcMasterComment 
        _ -> DidEditMcMasterComment 


wrapDidSendMcMasterComment : IncomingMessage -> DidSendMcMasterComment
wrapDidSendMcMasterComment x__ =
    case x__ of
        (MDidSendMcMasterComment comment)  -> (DidSendMcMasterComment comment) 
        _ -> DidSendMcMasterComment ""


wrapDidEditMcMasterNotice : IncomingMessage -> DidEditMcMasterNotice
wrapDidEditMcMasterNotice x__ =
    case x__ of
        MDidEditMcMasterNotice  -> DidEditMcMasterNotice 
        _ -> DidEditMcMasterNotice 


wrapDidPublishMcMasterNotice : IncomingMessage -> DidPublishMcMasterNotice
wrapDidPublishMcMasterNotice x__ =
    case x__ of
        (MDidPublishMcMasterNotice notice)  -> (DidPublishMcMasterNotice notice) 
        _ -> DidPublishMcMasterNotice (Notice "" "" "" 1548438211 "" [])


wrapNewMcMasterNotice : IncomingMessage -> NewMcMasterNotice
wrapNewMcMasterNotice x__ =
    case x__ of
        (MNewMcMasterNotice notice)  -> (NewMcMasterNotice notice) 
        _ -> NewMcMasterNotice (Notice "" "" "" 1548438211 "" [])



unwrapEnterMcMasterCreateNotice : EnterMcMasterCreateNotice -> OutgoingTransition
unwrapEnterMcMasterCreateNotice EnterMcMasterCreateNotice  = TEnterMcMasterCreateNotice 


unwrapCancelNotice : CancelNotice -> OutgoingTransition
unwrapCancelNotice CancelNotice  = TCancelNotice 


unwrapEnterUniversities : EnterUniversities -> OutgoingTransition
unwrapEnterUniversities EnterUniversities  = TEnterUniversities 


unwrapExitUniversities : ExitUniversities -> OutgoingTransition
unwrapExitUniversities ExitUniversities  = TExitUniversities 


unwrapEnterMcMasterUniversity : EnterMcMasterUniversity -> OutgoingTransition
unwrapEnterMcMasterUniversity EnterMcMasterUniversity  = TEnterMcMasterUniversity 


unwrapExitMcMasterUniversity : ExitMcMasterUniversity -> OutgoingTransition
unwrapExitMcMasterUniversity ExitMcMasterUniversity  = TExitMcMasterUniversity 


unwrapEditMcMasterComment : EditMcMasterComment -> OutgoingTransition
unwrapEditMcMasterComment (EditMcMasterComment partialComment)  = (TEditMcMasterComment partialComment) 


unwrapSendMcMasterComment : SendMcMasterComment -> OutgoingTransition
unwrapSendMcMasterComment (SendMcMasterComment comment)  = (TSendMcMasterComment comment) 


unwrapEditMcMasterNotice : EditMcMasterNotice -> OutgoingTransition
unwrapEditMcMasterNotice (EditMcMasterNotice partialNotice)  = (TEditMcMasterNotice partialNotice) 


unwrapPublishMcMasterNotice : PublishMcMasterNotice -> OutgoingTransition
unwrapPublishMcMasterNotice (PublishMcMasterNotice notice)  = (TPublishMcMasterNotice notice) 



