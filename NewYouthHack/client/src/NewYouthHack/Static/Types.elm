module NewYouthHack.Static.Types exposing(..)

-- the types of all places in the net
-- place states
type SquareOne  =
      SquareOne


type UniversitiesAndColleges  =
      UniversitiesAndColleges


type McMasterUniversity  =
      McMasterUniversity (List Notice {-notice-}) {-notices-} (List (Int {-uid-}, Int {-timestamp-}, String {-body-}) {-statement-}) {-statements-}


type McMasterCreateNotice  =
      McMasterCreateNotice Notice {-notice-}



-- union place type
type NetState  =
      SSquareOne SquareOne
    | SUniversitiesAndColleges UniversitiesAndColleges
    | SMcMasterUniversity McMasterUniversity
    | SMcMasterCreateNotice McMasterCreateNotice
-- server transition types
type OutgoingTransition  =
      TEnterMcMasterCreateNotice
    | TCancelNotice
    | TEnterUniversities
    | TExitUniversities
    | TEnterMcMasterUniversity
    | TExitMcMasterUniversity
    | TEditMcMasterComment String
    | TSendMcMasterComment String
    | TEditMcMasterNotice String
    | TPublishMcMasterNotice Notice
type EnterMcMasterCreateNotice  =
      EnterMcMasterCreateNotice
type CancelNotice  =
      CancelNotice
type EnterUniversities  =
      EnterUniversities
type ExitUniversities  =
      ExitUniversities
type EnterMcMasterUniversity  =
      EnterMcMasterUniversity
type ExitMcMasterUniversity  =
      ExitMcMasterUniversity
type EditMcMasterComment  =
      EditMcMasterComment String
type SendMcMasterComment  =
      SendMcMasterComment String
type EditMcMasterNotice  =
      EditMcMasterNotice String
type PublishMcMasterNotice  =
      PublishMcMasterNotice Notice

-- outgoing server message types
type DidEnterMcMasterCreateNotice  =
      DidEnterMcMasterCreateNotice
type DidCancelNotice  =
      DidCancelNotice
type DidEnterUniversitiesAndColleges  =
      DidEnterUniversitiesAndColleges
type DidLeaveUniversities  =
      DidLeaveUniversities
type DidEnterMcMasterUniversity  =
      DidEnterMcMasterUniversity
type DidLeaveMcMasterUniversity  =
      DidLeaveMcMasterUniversity
type DidEditMcMasterComment  =
      DidEditMcMasterComment
type DidSendMcMasterComment  =
      DidSendMcMasterComment String {-comment-}
type DidEditMcMasterNotice  =
      DidEditMcMasterNotice
type DidPublishMcMasterNotice  =
      DidPublishMcMasterNotice Notice {-notice-}
type NewMcMasterNotice  =
      NewMcMasterNotice Notice {-notice-}
type IncomingMessage  =
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

-- extra client types
type Notice  =
      Notice String {-title-} String {-contact-} String {-location-} Int {-timestamp-} String {-body-} (List (Int {-uid-}, String {-comment-}) {-uidComment-}) {-comments-}


