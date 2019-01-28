module NewYouthHack.Static.Update exposing(..)
import NewYouthHack.Static.Types exposing(..)
import NewYouthHack.Static.Wrappers exposing(..)
import NewYouthHack.Static.FromSuperPlace exposing (FromSuperPlace)
import NewYouthHack.Update exposing(..)

update : FromSuperPlace -> IncomingMessage -> NetState -> (NetState,Maybe (Cmd OutgoingTransition))
update fsp trans state =
    case (trans,state) of
        (MDidEnterMcMasterCreateNotice , SMcMasterUniversity st) -> (SMcMasterCreateNotice <| updateMcMasterUniversityDidEnterMcMasterCreateNoticeMcMasterCreateNotice fsp (wrapDidEnterMcMasterCreateNotice trans) st, Nothing)

        (MDidCancelNotice , SMcMasterCreateNotice st) -> (SMcMasterUniversity <| updateMcMasterCreateNoticeDidCancelNoticeMcMasterUniversity fsp (wrapDidCancelNotice trans) st, Nothing)

        (MDidEnterUniversitiesAndColleges , SSquareOne st) -> (SUniversitiesAndColleges <| updateSquareOneDidEnterUniversitiesAndCollegesUniversitiesAndColleges fsp (wrapDidEnterUniversitiesAndColleges trans) st, Nothing)

        (MDidLeaveUniversities , SUniversitiesAndColleges st) -> (SSquareOne <| updateUniversitiesAndCollegesDidLeaveUniversitiesSquareOne fsp (wrapDidLeaveUniversities trans) st, Nothing)

        (MDidEnterMcMasterUniversity , SUniversitiesAndColleges st) -> (SMcMasterUniversity <| updateUniversitiesAndCollegesDidEnterMcMasterUniversityMcMasterUniversity fsp (wrapDidEnterMcMasterUniversity trans) st, Nothing)

        (MDidLeaveMcMasterUniversity , SMcMasterUniversity st) -> (SUniversitiesAndColleges <| updateMcMasterUniversityDidLeaveMcMasterUniversityUniversitiesAndColleges fsp (wrapDidLeaveMcMasterUniversity trans) st, Nothing)

        (MDidEditMcMasterComment , SMcMasterUniversity st) -> (SMcMasterUniversity <| updateMcMasterUniversityDidEditMcMasterCommentMcMasterUniversity fsp (wrapDidEditMcMasterComment trans) st, Nothing)

        ((MDidSendMcMasterComment _) , SMcMasterUniversity st) -> (SMcMasterUniversity <| updateMcMasterUniversityDidSendMcMasterCommentMcMasterUniversity fsp (wrapDidSendMcMasterComment trans) st, Nothing)

        (MDidEditMcMasterNotice , SMcMasterCreateNotice st) -> (SMcMasterCreateNotice <| updateMcMasterCreateNoticeDidEditMcMasterNoticeMcMasterCreateNotice fsp (wrapDidEditMcMasterNotice trans) st, Nothing)

        ((MDidPublishMcMasterNotice _) , SPublishMcMasterNotice st) -> (SMcMasterUniversity <| updatePublishMcMasterNoticeDidPublishMcMasterNoticeMcMasterUniversity fsp (wrapDidPublishMcMasterNotice trans) st, Nothing)
        ((MNewMcMasterNotice _) , SMcMasterUniversity st) -> (SMcMasterUniversity <| updateMcMasterUniversityNewMcMasterNoticeMcMasterUniversity fsp (wrapNewMcMasterNotice trans) st, Nothing)


        _ -> (state, Nothing)
