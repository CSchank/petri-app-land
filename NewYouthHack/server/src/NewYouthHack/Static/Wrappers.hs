module NewYouthHack.Static.Wrappers where
import NewYouthHack.Static.Types

unwrapDidEnterMcMasterCreateNotice :: DidEnterMcMasterCreateNotice -> ClientMessage
unwrapDidEnterMcMasterCreateNotice DidEnterMcMasterCreateNotice  = MDidEnterMcMasterCreateNotice 


unwrapDidCancelNotice :: DidCancelNotice -> ClientMessage
unwrapDidCancelNotice DidCancelNotice  = MDidCancelNotice 


unwrapDidEnterUniversitiesAndColleges :: DidEnterUniversitiesAndColleges -> ClientMessage
unwrapDidEnterUniversitiesAndColleges DidEnterUniversitiesAndColleges  = MDidEnterUniversitiesAndColleges 


unwrapDidLeaveUniversities :: DidLeaveUniversities -> ClientMessage
unwrapDidLeaveUniversities DidLeaveUniversities  = MDidLeaveUniversities 


unwrapDidEnterMcMasterUniversity :: DidEnterMcMasterUniversity -> ClientMessage
unwrapDidEnterMcMasterUniversity DidEnterMcMasterUniversity  = MDidEnterMcMasterUniversity 


unwrapDidLeaveMcMasterUniversity :: DidLeaveMcMasterUniversity -> ClientMessage
unwrapDidLeaveMcMasterUniversity DidLeaveMcMasterUniversity  = MDidLeaveMcMasterUniversity 


unwrapDidEditMcMasterComment :: DidEditMcMasterComment -> ClientMessage
unwrapDidEditMcMasterComment DidEditMcMasterComment  = MDidEditMcMasterComment 


unwrapDidSendMcMasterComment :: DidSendMcMasterComment -> ClientMessage
unwrapDidSendMcMasterComment (DidSendMcMasterComment comment)  = (MDidSendMcMasterComment comment) 


unwrapDidEditMcMasterNotice :: DidEditMcMasterNotice -> ClientMessage
unwrapDidEditMcMasterNotice DidEditMcMasterNotice  = MDidEditMcMasterNotice 


unwrapDidPublishMcMasterNotice :: DidPublishMcMasterNotice -> ClientMessage
unwrapDidPublishMcMasterNotice (DidPublishMcMasterNotice notice)  = (MDidPublishMcMasterNotice notice) 


unwrapNewMcMasterNotice :: NewMcMasterNotice -> ClientMessage
unwrapNewMcMasterNotice (NewMcMasterNotice notice)  = (MNewMcMasterNotice notice) 



wrapDidEnterMcMasterCreateNotice :: ClientMessage -> DidEnterMcMasterCreateNotice
wrapDidEnterMcMasterCreateNotice x__ =
    case x__ of
        MDidEnterMcMasterCreateNotice  -> DidEnterMcMasterCreateNotice 
        _ -> DidEnterMcMasterCreateNotice 


wrapDidCancelNotice :: ClientMessage -> DidCancelNotice
wrapDidCancelNotice x__ =
    case x__ of
        MDidCancelNotice  -> DidCancelNotice 
        _ -> DidCancelNotice 


wrapDidEnterUniversitiesAndColleges :: ClientMessage -> DidEnterUniversitiesAndColleges
wrapDidEnterUniversitiesAndColleges x__ =
    case x__ of
        MDidEnterUniversitiesAndColleges  -> DidEnterUniversitiesAndColleges 
        _ -> DidEnterUniversitiesAndColleges 


wrapDidLeaveUniversities :: ClientMessage -> DidLeaveUniversities
wrapDidLeaveUniversities x__ =
    case x__ of
        MDidLeaveUniversities  -> DidLeaveUniversities 
        _ -> DidLeaveUniversities 


wrapDidEnterMcMasterUniversity :: ClientMessage -> DidEnterMcMasterUniversity
wrapDidEnterMcMasterUniversity x__ =
    case x__ of
        MDidEnterMcMasterUniversity  -> DidEnterMcMasterUniversity 
        _ -> DidEnterMcMasterUniversity 


wrapDidLeaveMcMasterUniversity :: ClientMessage -> DidLeaveMcMasterUniversity
wrapDidLeaveMcMasterUniversity x__ =
    case x__ of
        MDidLeaveMcMasterUniversity  -> DidLeaveMcMasterUniversity 
        _ -> DidLeaveMcMasterUniversity 


wrapDidEditMcMasterComment :: ClientMessage -> DidEditMcMasterComment
wrapDidEditMcMasterComment x__ =
    case x__ of
        MDidEditMcMasterComment  -> DidEditMcMasterComment 
        _ -> DidEditMcMasterComment 


wrapDidSendMcMasterComment :: ClientMessage -> DidSendMcMasterComment
wrapDidSendMcMasterComment x__ =
    case x__ of
        (MDidSendMcMasterComment comment)  -> (DidSendMcMasterComment comment) 
        _ -> DidSendMcMasterComment ""


wrapDidEditMcMasterNotice :: ClientMessage -> DidEditMcMasterNotice
wrapDidEditMcMasterNotice x__ =
    case x__ of
        MDidEditMcMasterNotice  -> DidEditMcMasterNotice 
        _ -> DidEditMcMasterNotice 


wrapDidPublishMcMasterNotice :: ClientMessage -> DidPublishMcMasterNotice
wrapDidPublishMcMasterNotice x__ =
    case x__ of
        (MDidPublishMcMasterNotice notice)  -> (DidPublishMcMasterNotice notice) 
        _ -> DidPublishMcMasterNotice (Notice "" "" "" 1548438211 "" [])


wrapNewMcMasterNotice :: ClientMessage -> NewMcMasterNotice
wrapNewMcMasterNotice x__ =
    case x__ of
        (MNewMcMasterNotice notice)  -> (NewMcMasterNotice notice) 
        _ -> NewMcMasterNotice (Notice "" "" "" 1548438211 "" [])



unwrapSquareOnePlayer :: SquareOnePlayer -> Player
unwrapSquareOnePlayer SquareOnePlayer  = PSquareOnePlayer 


unwrapUniversitiesAndCollegesPlayer :: UniversitiesAndCollegesPlayer -> Player
unwrapUniversitiesAndCollegesPlayer (UniversitiesAndCollegesPlayer clientID)  = (PUniversitiesAndCollegesPlayer clientID) 


unwrapMcMasterUniversityPlayer :: McMasterUniversityPlayer -> Player
unwrapMcMasterUniversityPlayer (McMasterUniversityPlayer clientID)  = (PMcMasterUniversityPlayer clientID) 


unwrapMcMasterCreateNoticePlayer :: McMasterCreateNoticePlayer -> Player
unwrapMcMasterCreateNoticePlayer (McMasterCreateNoticePlayer clientID)  = (PMcMasterCreateNoticePlayer clientID) 



wrapSquareOnePlayer :: Player -> SquareOnePlayer
wrapSquareOnePlayer x__ =
    case x__ of
        PSquareOnePlayer  -> SquareOnePlayer 
        _ -> SquareOnePlayer 


wrapUniversitiesAndCollegesPlayer :: Player -> UniversitiesAndCollegesPlayer
wrapUniversitiesAndCollegesPlayer x__ =
    case x__ of
        (PUniversitiesAndCollegesPlayer clientID)  -> (UniversitiesAndCollegesPlayer clientID) 
        _ -> UniversitiesAndCollegesPlayer 0


wrapMcMasterUniversityPlayer :: Player -> McMasterUniversityPlayer
wrapMcMasterUniversityPlayer x__ =
    case x__ of
        (PMcMasterUniversityPlayer clientID)  -> (McMasterUniversityPlayer clientID) 
        _ -> McMasterUniversityPlayer 0


wrapMcMasterCreateNoticePlayer :: Player -> McMasterCreateNoticePlayer
wrapMcMasterCreateNoticePlayer x__ =
    case x__ of
        (PMcMasterCreateNoticePlayer clientID)  -> (McMasterCreateNoticePlayer clientID) 
        _ -> McMasterCreateNoticePlayer 0



unwrapEnterMcMasterCreateNoticefromMcMasterUniversity :: EnterMcMasterCreateNoticefromMcMasterUniversity -> (Player, Maybe ClientMessage)
unwrapEnterMcMasterCreateNoticefromMcMasterUniversity trans =
    case trans of
        (EnterMcMasterCreateNotice_McMasterUniversitytoMcMasterUniversity player)  -> (unwrapMcMasterUniversityPlayer player, Nothing)
        (EnterMcMasterCreateNotice_McMasterUniversitytoMcMasterCreateNotice player msg)  -> (unwrapMcMasterCreateNoticePlayer player, Just $ unwrapDidEnterMcMasterCreateNotice msg)




unwrapCancelNoticefromMcMasterCreateNotice :: CancelNoticefromMcMasterCreateNotice -> (Player, Maybe ClientMessage)
unwrapCancelNoticefromMcMasterCreateNotice trans =
    case trans of
        (CancelNotice_McMasterCreateNoticetoMcMasterCreateNotice player)  -> (unwrapMcMasterCreateNoticePlayer player, Nothing)
        (CancelNotice_McMasterCreateNoticetoMcMasterUniversity player msg)  -> (unwrapMcMasterUniversityPlayer player, Just $ unwrapDidCancelNotice msg)




unwrapEnterUniversitiesfromSquareOne :: EnterUniversitiesfromSquareOne -> (Player, Maybe ClientMessage)
unwrapEnterUniversitiesfromSquareOne trans =
    case trans of
        (EnterUniversities_SquareOnetoSquareOne player)  -> (unwrapSquareOnePlayer player, Nothing)
        (EnterUniversities_SquareOnetoUniversitiesAndColleges player msg)  -> (unwrapUniversitiesAndCollegesPlayer player, Just $ unwrapDidEnterUniversitiesAndColleges msg)




unwrapExitUniversitiesfromUniversitiesAndColleges :: ExitUniversitiesfromUniversitiesAndColleges -> (Player, Maybe ClientMessage)
unwrapExitUniversitiesfromUniversitiesAndColleges trans =
    case trans of
        (ExitUniversities_UniversitiesAndCollegestoUniversitiesAndColleges player)  -> (unwrapUniversitiesAndCollegesPlayer player, Nothing)
        (ExitUniversities_UniversitiesAndCollegestoSquareOne player msg)  -> (unwrapSquareOnePlayer player, Just $ unwrapDidLeaveUniversities msg)




unwrapEnterMcMasterUniversityfromUniversitiesAndColleges :: EnterMcMasterUniversityfromUniversitiesAndColleges -> (Player, Maybe ClientMessage)
unwrapEnterMcMasterUniversityfromUniversitiesAndColleges trans =
    case trans of
        (EnterMcMasterUniversity_UniversitiesAndCollegestoUniversitiesAndColleges player)  -> (unwrapUniversitiesAndCollegesPlayer player, Nothing)
        (EnterMcMasterUniversity_UniversitiesAndCollegestoMcMasterUniversity player msg)  -> (unwrapMcMasterUniversityPlayer player, Just $ unwrapDidEnterMcMasterUniversity msg)




unwrapExitMcMasterUniversityfromMcMasterUniversity :: ExitMcMasterUniversityfromMcMasterUniversity -> (Player, Maybe ClientMessage)
unwrapExitMcMasterUniversityfromMcMasterUniversity trans =
    case trans of
        (ExitMcMasterUniversity_McMasterUniversitytoMcMasterUniversity player)  -> (unwrapMcMasterUniversityPlayer player, Nothing)
        (ExitMcMasterUniversity_McMasterUniversitytoUniversitiesAndColleges player msg)  -> (unwrapUniversitiesAndCollegesPlayer player, Just $ unwrapDidLeaveMcMasterUniversity msg)




unwrapEditMcMasterCommentfromMcMasterUniversity :: EditMcMasterCommentfromMcMasterUniversity -> (Player, Maybe ClientMessage)
unwrapEditMcMasterCommentfromMcMasterUniversity trans =
    case trans of
        (EditMcMasterComment_McMasterUniversitytoMcMasterUniversity player)  -> (unwrapMcMasterUniversityPlayer player, Nothing)
        (EditMcMasterComment_McMasterUniversitytoMcMasterUniversity player msg)  -> (unwrapMcMasterUniversityPlayer player, Just $ unwrapDidEditMcMasterComment msg)




unwrapSendMcMasterCommentfromMcMasterUniversity :: SendMcMasterCommentfromMcMasterUniversity -> (Player, Maybe ClientMessage)
unwrapSendMcMasterCommentfromMcMasterUniversity trans =
    case trans of
        (SendMcMasterComment_McMasterUniversitytoMcMasterUniversity player msg)  -> (unwrapMcMasterUniversityPlayer player, Just $ unwrapDidSendMcMasterComment msg)




unwrapEditMcMasterNoticefromMcMasterCreateNotice :: EditMcMasterNoticefromMcMasterCreateNotice -> (Player, Maybe ClientMessage)
unwrapEditMcMasterNoticefromMcMasterCreateNotice trans =
    case trans of
        (EditMcMasterNotice_McMasterCreateNoticetoMcMasterCreateNotice player)  -> (unwrapMcMasterCreateNoticePlayer player, Nothing)
        (EditMcMasterNotice_McMasterCreateNoticetoMcMasterCreateNotice player msg)  -> (unwrapMcMasterCreateNoticePlayer player, Just $ unwrapDidEditMcMasterNotice msg)




unwrapPublishMcMasterNoticefromMcMasterUniversity :: PublishMcMasterNoticefromMcMasterUniversity -> (Player, Maybe ClientMessage)
unwrapPublishMcMasterNoticefromMcMasterUniversity trans =
    case trans of
        (PublishMcMasterNotice_McMasterUniversitytoMcMasterUniversity player msg)  -> (unwrapMcMasterUniversityPlayer player, Just $ unwrapNewMcMasterNotice msg)



unwrapPublishMcMasterNoticefromPublishMcMasterNotice :: PublishMcMasterNoticefromPublishMcMasterNotice -> (Player, Maybe ClientMessage)
unwrapPublishMcMasterNoticefromPublishMcMasterNotice trans =
    case trans of
        (PublishMcMasterNotice_PublishMcMasterNoticetoPublishMcMasterNotice player)  -> (unwrapPublishMcMasterNoticePlayer player, Nothing)
        (PublishMcMasterNotice_PublishMcMasterNoticetoMcMasterUniversity player msg)  -> (unwrapMcMasterUniversityPlayer player, Just $ unwrapDidPublishMcMasterNotice msg)





unwrapEnterMcMasterCreateNotice :: EnterMcMasterCreateNotice -> Transition
unwrapEnterMcMasterCreateNotice EnterMcMasterCreateNotice  = TEnterMcMasterCreateNotice 


unwrapCancelNotice :: CancelNotice -> Transition
unwrapCancelNotice CancelNotice  = TCancelNotice 


unwrapEnterUniversities :: EnterUniversities -> Transition
unwrapEnterUniversities EnterUniversities  = TEnterUniversities 


unwrapExitUniversities :: ExitUniversities -> Transition
unwrapExitUniversities ExitUniversities  = TExitUniversities 


unwrapEnterMcMasterUniversity :: EnterMcMasterUniversity -> Transition
unwrapEnterMcMasterUniversity EnterMcMasterUniversity  = TEnterMcMasterUniversity 


unwrapExitMcMasterUniversity :: ExitMcMasterUniversity -> Transition
unwrapExitMcMasterUniversity ExitMcMasterUniversity  = TExitMcMasterUniversity 


unwrapEditMcMasterComment :: EditMcMasterComment -> Transition
unwrapEditMcMasterComment (EditMcMasterComment partialComment)  = (TEditMcMasterComment partialComment) 


unwrapSendMcMasterComment :: SendMcMasterComment -> Transition
unwrapSendMcMasterComment (SendMcMasterComment comment)  = (TSendMcMasterComment comment) 


unwrapEditMcMasterNotice :: EditMcMasterNotice -> Transition
unwrapEditMcMasterNotice (EditMcMasterNotice partialNotice)  = (TEditMcMasterNotice partialNotice) 


unwrapPublishMcMasterNotice :: PublishMcMasterNotice -> Transition
unwrapPublishMcMasterNotice (PublishMcMasterNotice notice)  = (TPublishMcMasterNotice notice) 



wrapEnterMcMasterCreateNotice :: Transition -> EnterMcMasterCreateNotice
wrapEnterMcMasterCreateNotice x__ =
    case x__ of
        TEnterMcMasterCreateNotice  -> EnterMcMasterCreateNotice 
        _ -> EnterMcMasterCreateNotice 


wrapCancelNotice :: Transition -> CancelNotice
wrapCancelNotice x__ =
    case x__ of
        TCancelNotice  -> CancelNotice 
        _ -> CancelNotice 


wrapEnterUniversities :: Transition -> EnterUniversities
wrapEnterUniversities x__ =
    case x__ of
        TEnterUniversities  -> EnterUniversities 
        _ -> EnterUniversities 


wrapExitUniversities :: Transition -> ExitUniversities
wrapExitUniversities x__ =
    case x__ of
        TExitUniversities  -> ExitUniversities 
        _ -> ExitUniversities 


wrapEnterMcMasterUniversity :: Transition -> EnterMcMasterUniversity
wrapEnterMcMasterUniversity x__ =
    case x__ of
        TEnterMcMasterUniversity  -> EnterMcMasterUniversity 
        _ -> EnterMcMasterUniversity 


wrapExitMcMasterUniversity :: Transition -> ExitMcMasterUniversity
wrapExitMcMasterUniversity x__ =
    case x__ of
        TExitMcMasterUniversity  -> ExitMcMasterUniversity 
        _ -> ExitMcMasterUniversity 


wrapEditMcMasterComment :: Transition -> EditMcMasterComment
wrapEditMcMasterComment x__ =
    case x__ of
        (TEditMcMasterComment partialComment)  -> (EditMcMasterComment partialComment) 
        _ -> EditMcMasterComment ""


wrapSendMcMasterComment :: Transition -> SendMcMasterComment
wrapSendMcMasterComment x__ =
    case x__ of
        (TSendMcMasterComment comment)  -> (SendMcMasterComment comment) 
        _ -> SendMcMasterComment ""


wrapEditMcMasterNotice :: Transition -> EditMcMasterNotice
wrapEditMcMasterNotice x__ =
    case x__ of
        (TEditMcMasterNotice partialNotice)  -> (EditMcMasterNotice partialNotice) 
        _ -> EditMcMasterNotice ""


wrapPublishMcMasterNotice :: Transition -> PublishMcMasterNotice
wrapPublishMcMasterNotice x__ =
    case x__ of
        (TPublishMcMasterNotice notice)  -> (PublishMcMasterNotice notice) 
        _ -> PublishMcMasterNotice (Notice "" "" "" 1548438211 "" [])



