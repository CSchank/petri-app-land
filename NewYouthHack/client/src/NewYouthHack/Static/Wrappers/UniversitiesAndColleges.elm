module NewYouthHack.Static.Wrappers.UniversitiesAndColleges exposing(..)
import NewYouthHack.Static.Types.UniversitiesAndColleges exposing(..)
import NewYouthHack.Static.Types exposing(..)

unwrap : Msg -> OutgoingTransition
unwrap msg =
    case msg of
        NewYouthHack.Static.Types.UniversitiesAndColleges.ExitUniversities  -> TExitUniversities 
        NewYouthHack.Static.Types.UniversitiesAndColleges.EnterMcMasterUniversity  -> TEnterMcMasterUniversity 

