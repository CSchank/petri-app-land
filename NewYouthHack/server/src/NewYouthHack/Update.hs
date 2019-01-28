module NewYouthHack.Update where
import NewYouthHack.Static.Types
import NewYouthHack.Static.FromSuperPlace
import Static.List
import Utils.Utils
import Static.ServerTypes

-- function called when new client connects (do not delete)
clientConnect :: FromSuperPlace -> ClientID -> SquareOne -> (SquareOne, SquareOnePlayer)
clientConnect fsp clientID squareOne =
    error "Please fill out clientConnect function for the NewYouthHack net."

-- functions called when a client disconnects (do not delete)
clientDisconnectFromMcMasterCreateNotice :: FromSuperPlace -> ClientID -> McMasterCreateNotice -> McMasterCreateNoticePlayer -> McMasterCreateNotice
clientDisconnectFromMcMasterCreateNotice fsp clientID mcMasterCreateNotice mcMasterCreateNoticePlayer =
    error "Please fill out the clientDisconnectFromMcMasterCreateNotice function for the NewYouthHack net."

clientDisconnectFromMcMasterUniversity :: FromSuperPlace -> ClientID -> McMasterUniversity -> McMasterUniversityPlayer -> McMasterUniversity
clientDisconnectFromMcMasterUniversity fsp clientID mcMasterUniversity mcMasterUniversityPlayer =
    error "Please fill out the clientDisconnectFromMcMasterUniversity function for the NewYouthHack net."

clientDisconnectFromSquareOne :: FromSuperPlace -> ClientID -> SquareOne -> SquareOnePlayer -> SquareOne
clientDisconnectFromSquareOne fsp clientID squareOne squareOnePlayer =
    error "Please fill out the clientDisconnectFromSquareOne function for the NewYouthHack net."

clientDisconnectFromUniversitiesAndColleges :: FromSuperPlace -> ClientID -> UniversitiesAndColleges -> UniversitiesAndCollegesPlayer -> UniversitiesAndColleges
clientDisconnectFromUniversitiesAndColleges fsp clientID universitiesAndColleges universitiesAndCollegesPlayer =
    error "Please fill out the clientDisconnectFromUniversitiesAndColleges function for the NewYouthHack net."


-- functions for each transition
updateEnterMcMasterCreateNotice :: FromSuperPlace -> ClientID -> EnterMcMasterCreateNotice -> McMasterUniversity -> McMasterCreateNotice -> List McMasterUniversityPlayer -> (McMasterUniversity, McMasterCreateNotice, McMasterUniversityPlayer -> EnterMcMasterCreateNoticefromMcMasterUniversity)
updateEnterMcMasterCreateNotice fsp clientId EnterMcMasterCreateNotice mcMasterUniversity mcMasterCreateNotice lstMcMasterUniversity =
    let
        fromMcMasterUniversity :: McMasterUniversityPlayer -> EnterMcMasterCreateNoticefromMcMasterUniversity
        fromMcMasterUniversity (McMasterUniversityPlayer pID) =
            if pID == clientId then
                EnterMcMasterCreateNotice_McMasterUniversitytoMcMasterCreateNotice (McMasterCreateNoticePlayer pID) DidEnterMcMasterCreateNotice 
            else
                EnterMcMasterCreateNotice_McMasterUniversitytoMcMasterUniversity (McMasterUniversityPlayer pID)

    in
        (mcMasterUniversity, mcMasterCreateNotice, fromMcMasterUniversity)

updateCancelNotice :: FromSuperPlace -> ClientID -> CancelNotice -> McMasterCreateNotice -> McMasterUniversity -> List McMasterCreateNoticePlayer -> (McMasterCreateNotice, McMasterUniversity, McMasterCreateNoticePlayer -> CancelNoticefromMcMasterCreateNotice)
updateCancelNotice fsp clientId CancelNotice mcMasterCreateNotice mcMasterUniversity lstMcMasterCreateNotice =
    let
        fromMcMasterCreateNotice :: McMasterCreateNoticePlayer -> CancelNoticefromMcMasterCreateNotice
        fromMcMasterCreateNotice pmcMasterCreateNotice = error "Please fill in function stub."


    in
        (mcMasterCreateNotice, mcMasterUniversity, fromMcMasterCreateNotice)

updateEnterUniversities :: FromSuperPlace -> ClientID -> EnterUniversities -> SquareOne -> UniversitiesAndColleges -> List SquareOnePlayer -> (SquareOne, UniversitiesAndColleges, SquareOnePlayer -> EnterUniversitiesfromSquareOne)
updateEnterUniversities fsp clientId EnterUniversities squareOne universitiesAndColleges lstSquareOne =
    let
        fromSquareOne :: SquareOnePlayer -> EnterUniversitiesfromSquareOne
        fromSquareOne psquareOne = error "Please fill in function stub."


    in
        (squareOne, universitiesAndColleges, fromSquareOne)

updateExitUniversities :: FromSuperPlace -> ClientID -> ExitUniversities -> UniversitiesAndColleges -> SquareOne -> List UniversitiesAndCollegesPlayer -> (UniversitiesAndColleges, SquareOne, UniversitiesAndCollegesPlayer -> ExitUniversitiesfromUniversitiesAndColleges)
updateExitUniversities fsp clientId ExitUniversities universitiesAndColleges squareOne lstUniversitiesAndColleges =
    let
        fromUniversitiesAndColleges :: UniversitiesAndCollegesPlayer -> ExitUniversitiesfromUniversitiesAndColleges
        fromUniversitiesAndColleges puniversitiesAndColleges = error "Please fill in function stub."


    in
        (universitiesAndColleges, squareOne, fromUniversitiesAndColleges)

updateEnterMcMasterUniversity :: FromSuperPlace -> ClientID -> EnterMcMasterUniversity -> UniversitiesAndColleges -> McMasterUniversity -> List UniversitiesAndCollegesPlayer -> (UniversitiesAndColleges, McMasterUniversity, UniversitiesAndCollegesPlayer -> EnterMcMasterUniversityfromUniversitiesAndColleges)
updateEnterMcMasterUniversity fsp clientId EnterMcMasterUniversity universitiesAndColleges mcMasterUniversity lstUniversitiesAndColleges =
    let
        fromUniversitiesAndColleges :: UniversitiesAndCollegesPlayer -> EnterMcMasterUniversityfromUniversitiesAndColleges
        fromUniversitiesAndColleges puniversitiesAndColleges = error "Please fill in function stub."


    in
        (universitiesAndColleges, mcMasterUniversity, fromUniversitiesAndColleges)

updateExitMcMasterUniversity :: FromSuperPlace -> ClientID -> ExitMcMasterUniversity -> McMasterUniversity -> UniversitiesAndColleges -> List McMasterUniversityPlayer -> (McMasterUniversity, UniversitiesAndColleges, McMasterUniversityPlayer -> ExitMcMasterUniversityfromMcMasterUniversity)
updateExitMcMasterUniversity fsp clientId ExitMcMasterUniversity mcMasterUniversity universitiesAndColleges lstMcMasterUniversity =
    let
        fromMcMasterUniversity :: McMasterUniversityPlayer -> ExitMcMasterUniversityfromMcMasterUniversity
        fromMcMasterUniversity pmcMasterUniversity = error "Please fill in function stub."


    in
        (mcMasterUniversity, universitiesAndColleges, fromMcMasterUniversity)

updateEditMcMasterComment :: FromSuperPlace -> ClientID -> EditMcMasterComment -> McMasterUniversity -> List McMasterUniversityPlayer -> (McMasterUniversity, McMasterUniversityPlayer -> EditMcMasterCommentfromMcMasterUniversity)
updateEditMcMasterComment fsp clientId (EditMcMasterComment partialComment) mcMasterUniversity lstMcMasterUniversity =
    let
        fromMcMasterUniversity :: McMasterUniversityPlayer -> EditMcMasterCommentfromMcMasterUniversity
        fromMcMasterUniversity pmcMasterUniversity = error "Please fill in function stub."


    in
        (mcMasterUniversity, fromMcMasterUniversity)

updateSendMcMasterComment :: FromSuperPlace -> ClientID -> SendMcMasterComment -> McMasterUniversity -> List McMasterUniversityPlayer -> (McMasterUniversity, McMasterUniversityPlayer -> SendMcMasterCommentfromMcMasterUniversity)
updateSendMcMasterComment fsp clientId (SendMcMasterComment comment) mcMasterUniversity lstMcMasterUniversity =
    let
        fromMcMasterUniversity :: McMasterUniversityPlayer -> SendMcMasterCommentfromMcMasterUniversity
        fromMcMasterUniversity pmcMasterUniversity = error "Please fill in function stub."


    in
        (mcMasterUniversity, fromMcMasterUniversity)

updateEditMcMasterNotice :: FromSuperPlace -> ClientID -> EditMcMasterNotice -> McMasterCreateNotice -> List McMasterCreateNoticePlayer -> (McMasterCreateNotice, McMasterCreateNoticePlayer -> EditMcMasterNoticefromMcMasterCreateNotice)
updateEditMcMasterNotice fsp clientId (EditMcMasterNotice partialNotice) mcMasterCreateNotice lstMcMasterCreateNotice =
    let
        fromMcMasterCreateNotice :: McMasterCreateNoticePlayer -> EditMcMasterNoticefromMcMasterCreateNotice
        fromMcMasterCreateNotice pmcMasterCreateNotice = error "Please fill in function stub."


    in
        (mcMasterCreateNotice, fromMcMasterCreateNotice)

updatePublishMcMasterNotice :: FromSuperPlace -> ClientID -> PublishMcMasterNotice -> PublishMcMasterNotice -> McMasterUniversity -> List PublishMcMasterNoticePlayer -> List McMasterUniversityPlayer -> (PublishMcMasterNotice, McMasterUniversity, McMasterUniversityPlayer -> PublishMcMasterNoticefromMcMasterUniversity, PublishMcMasterNoticePlayer -> PublishMcMasterNoticefromPublishMcMasterNotice)
updatePublishMcMasterNotice fsp clientId (PublishMcMasterNotice notice) publishMcMasterNotice mcMasterUniversity lstPublishMcMasterNotice lstMcMasterUniversity =
    let
        (currentTime,_) = fsp
        Notice title contact location _timestamp body _comments = notice
        noticeToSend = Notice title contact location currentTime body []

        fromMcMasterUniversity :: McMasterUniversityPlayer -> PublishMcMasterNoticefromMcMasterUniversity
        fromMcMasterUniversity pmcMasterUniversity = 
            PublishMcMasterNotice_McMasterUniversitytoMcMasterUniversity pmcMasterUniversity (NewMcMasterNotice noticeToSend)

        fromPublishMcMasterNotice :: PublishMcMasterNoticePlayer -> PublishMcMasterNoticefromPublishMcMasterNotice
        fromPublishMcMasterNotice (PublishMcMasterNoticePlayer pId) =
            if clientId == pId then
                PublishMcMasterNotice_PublishMcMasterNoticetoMcMasterUniversity (McMasterUniversityPlayer pId) (DidPublishMcMasterNotice noticeToSend)
            else
                PublishMcMasterNotice_PublishMcMasterNoticetoPublishMcMasterNotice (PublishMcMasterNoticePlayer pId)

    in
        (publishMcMasterNotice, mcMasterUniversity, fromPublishMcMasterNotice, fromMcMasterUniversity)


