module NewYouthHack.Static.Types where
import Data.Typeable (Typeable)
import Static.List

-- the initial state of all places in this net
-- place states and place player states
data SquareOne  =
      SquareOne (List Notice {-notice-}) {-notices-}
    deriving(Ord,Eq,Show,Typeable)

data SquareOnePlayer  =
      SquareOnePlayer
    deriving(Ord,Eq,Show,Typeable)


data UniversitiesAndColleges  =
      UniversitiesAndColleges (List Notice {-notice-}) {-notices-}
    deriving(Ord,Eq,Show,Typeable)

data UniversitiesAndCollegesPlayer  =
      UniversitiesAndCollegesPlayer Int {-clientID-}
    deriving(Ord,Eq,Show,Typeable)


data McMasterUniversity  =
      McMasterUniversity (List Notice {-notice-}) {-notices-} (List (Int {-uid-}, Int {-timestamp-}, String {-body-}) {-statement-}) {-statements-}
    deriving(Ord,Eq,Show,Typeable)

data McMasterUniversityPlayer  =
      McMasterUniversityPlayer Int {-clientID-}
    deriving(Ord,Eq,Show,Typeable)


data McMasterCreateNotice  =
      McMasterCreateNotice
    deriving(Ord,Eq,Show,Typeable)

data McMasterCreateNoticePlayer  =
      McMasterCreateNoticePlayer Int {-clientID-}
    deriving(Ord,Eq,Show,Typeable)



-- outgoing client message types
data DidEnterMcMasterCreateNotice  =
      DidEnterMcMasterCreateNotice
    deriving(Ord,Eq,Show)
data DidCancelNotice  =
      DidCancelNotice
    deriving(Ord,Eq,Show)
data DidEnterUniversitiesAndColleges  =
      DidEnterUniversitiesAndColleges
    deriving(Ord,Eq,Show)
data DidLeaveUniversities  =
      DidLeaveUniversities
    deriving(Ord,Eq,Show)
data DidEnterMcMasterUniversity  =
      DidEnterMcMasterUniversity
    deriving(Ord,Eq,Show)
data DidLeaveMcMasterUniversity  =
      DidLeaveMcMasterUniversity
    deriving(Ord,Eq,Show)
data DidEditMcMasterComment  =
      DidEditMcMasterComment
    deriving(Ord,Eq,Show)
data DidSendMcMasterComment  =
      DidSendMcMasterComment String {-comment-}
    deriving(Ord,Eq,Show)
data DidEditMcMasterNotice  =
      DidEditMcMasterNotice
    deriving(Ord,Eq,Show)
data DidPublishMcMasterNotice  =
      DidPublishMcMasterNotice Notice {-notice-}
    deriving(Ord,Eq,Show)
data NewMcMasterNotice  =
      NewMcMasterNotice Notice {-notice-}
    deriving(Ord,Eq,Show)
data ClientMessage  =
      MDidEnterMcMasterCreateNotice
    | MDidCancelNotice
    | MDidEnterUniversitiesAndColleges
    | MDidLeaveUniversities
    | MDidEnterMcMasterUniversity
    | MDidLeaveMcMasterUniversity
    | MDidEditMcMasterComment
    | MDidSendMcMasterComment String {-comment-}
    | MDidEditMcMasterNotice
    | MDidPublishMcMasterNotice Notice {-notice-}
    | MNewMcMasterNotice Notice {-notice-}
    deriving(Ord,Eq,Show)

-- individual transition types
data EnterMcMasterCreateNoticefromMcMasterUniversity  =
      EnterMcMasterCreateNotice_McMasterUniversitytoMcMasterUniversity McMasterUniversityPlayer
    | EnterMcMasterCreateNotice_McMasterUniversitytoMcMasterCreateNotice McMasterCreateNoticePlayer DidEnterMcMasterCreateNotice
    deriving(Ord,Eq,Show)

data CancelNoticefromMcMasterCreateNotice  =
      CancelNotice_McMasterCreateNoticetoMcMasterCreateNotice McMasterCreateNoticePlayer
    | CancelNotice_McMasterCreateNoticetoMcMasterUniversity McMasterUniversityPlayer DidCancelNotice
    deriving(Ord,Eq,Show)

data EnterUniversitiesfromSquareOne  =
      EnterUniversities_SquareOnetoSquareOne SquareOnePlayer
    | EnterUniversities_SquareOnetoUniversitiesAndColleges UniversitiesAndCollegesPlayer DidEnterUniversitiesAndColleges
    deriving(Ord,Eq,Show)

data ExitUniversitiesfromUniversitiesAndColleges  =
      ExitUniversities_UniversitiesAndCollegestoUniversitiesAndColleges UniversitiesAndCollegesPlayer
    | ExitUniversities_UniversitiesAndCollegestoSquareOne SquareOnePlayer DidLeaveUniversities
    deriving(Ord,Eq,Show)

data EnterMcMasterUniversityfromUniversitiesAndColleges  =
      EnterMcMasterUniversity_UniversitiesAndCollegestoUniversitiesAndColleges UniversitiesAndCollegesPlayer
    | EnterMcMasterUniversity_UniversitiesAndCollegestoMcMasterUniversity McMasterUniversityPlayer DidEnterMcMasterUniversity
    deriving(Ord,Eq,Show)

data ExitMcMasterUniversityfromMcMasterUniversity  =
      ExitMcMasterUniversity_McMasterUniversitytoMcMasterUniversity McMasterUniversityPlayer
    | ExitMcMasterUniversity_McMasterUniversitytoUniversitiesAndColleges UniversitiesAndCollegesPlayer DidLeaveMcMasterUniversity
    deriving(Ord,Eq,Show)

data EditMcMasterCommentfromMcMasterUniversity  =
      EditMcMasterComment_McMasterUniversitytoMcMasterUniversity McMasterUniversityPlayer
    | EditMcMasterComment_McMasterUniversitytoMcMasterUniversity McMasterUniversityPlayer DidEditMcMasterComment
    deriving(Ord,Eq,Show)

data SendMcMasterCommentfromMcMasterUniversity  =
      SendMcMasterComment_McMasterUniversitytoMcMasterUniversity McMasterUniversityPlayer DidSendMcMasterComment
    deriving(Ord,Eq,Show)

data EditMcMasterNoticefromMcMasterCreateNotice  =
      EditMcMasterNotice_McMasterCreateNoticetoMcMasterCreateNotice McMasterCreateNoticePlayer
    | EditMcMasterNotice_McMasterCreateNoticetoMcMasterCreateNotice McMasterCreateNoticePlayer DidEditMcMasterNotice
    deriving(Ord,Eq,Show)

data PublishMcMasterNoticefromMcMasterUniversity  =
      PublishMcMasterNotice_McMasterUniversitytoMcMasterUniversity McMasterUniversityPlayer NewMcMasterNotice
    deriving(Ord,Eq,Show)
data PublishMcMasterNoticefromPublishMcMasterNotice  =
      PublishMcMasterNotice_PublishMcMasterNoticetoPublishMcMasterNotice PublishMcMasterNoticePlayer
    | PublishMcMasterNotice_PublishMcMasterNoticetoMcMasterUniversity McMasterUniversityPlayer DidPublishMcMasterNotice
    deriving(Ord,Eq,Show)


-- main transition types
data Transition  =
      TEnterMcMasterCreateNotice
    | TCancelNotice
    | TEnterUniversities
    | TExitUniversities
    | TEnterMcMasterUniversity
    | TExitMcMasterUniversity
    | TEditMcMasterComment String {-partialComment-}
    | TSendMcMasterComment String {-comment-}
    | TEditMcMasterNotice String {-partialNotice-}
    | TPublishMcMasterNotice Notice {-notice-}
    deriving(Ord,Eq,Show)
data EnterMcMasterCreateNotice  =
      EnterMcMasterCreateNotice
    deriving(Ord,Eq,Show)
data CancelNotice  =
      CancelNotice
    deriving(Ord,Eq,Show)
data EnterUniversities  =
      EnterUniversities
    deriving(Ord,Eq,Show)
data ExitUniversities  =
      ExitUniversities
    deriving(Ord,Eq,Show)
data EnterMcMasterUniversity  =
      EnterMcMasterUniversity
    deriving(Ord,Eq,Show)
data ExitMcMasterUniversity  =
      ExitMcMasterUniversity
    deriving(Ord,Eq,Show)
data EditMcMasterComment  =
      EditMcMasterComment String {-partialComment-}
    deriving(Ord,Eq,Show)
data SendMcMasterComment  =
      SendMcMasterComment String {-comment-}
    deriving(Ord,Eq,Show)
data EditMcMasterNotice  =
      EditMcMasterNotice String {-partialNotice-}
    deriving(Ord,Eq,Show)
data PublishMcMasterNotice  =
      PublishMcMasterNotice Notice {-notice-}
    deriving(Ord,Eq,Show)

-- player state union type
data Player  =
      PSquareOnePlayer
    | PUniversitiesAndCollegesPlayer Int
    | PMcMasterUniversityPlayer Int
    | PMcMasterCreateNoticePlayer Int
    deriving(Ord,Eq,Show)
-- extra server types
data Notice  =
      Notice String {-title-} String {-contact-} String {-location-} Int {-timestamp-} String {-body-} (List (Int {-uid-}, String {-comment-}) {-uidComment-}) {-comments-}
    deriving(Ord,Eq,Show)


-- the FromSuperPlace type
