module NewYouthHack.Static.Types.McMasterUniversity exposing(..)
import NewYouthHack.Static.Types exposing(..)

type Msg  =
      EnterMcMasterCreateNotice
    | ExitMcMasterUniversity
    | EditMcMasterComment String
    | SendMcMasterComment String
    | PublishMcMasterNotice Notice
