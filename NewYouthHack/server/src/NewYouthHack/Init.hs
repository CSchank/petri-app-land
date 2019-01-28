module NewYouthHack.Init where
import NewYouthHack.Static.Types

-- the initial states of each place in this net
initSquareOne :: SquareOne
initSquareOne = SquareOne []

initUniversitiesAndColleges :: UniversitiesAndColleges
initUniversitiesAndColleges = UniversitiesAndColleges []

initMcMasterUniversity :: McMasterUniversity
initMcMasterUniversity = McMasterUniversity [Notice "First Notice" "robot" "ETB" 7777 "Message from Time-Travelling Robot." []] []

-- Notice String String String Int String (List (Int, String))

initMcMasterCreateNotice :: McMasterCreateNotice
initMcMasterCreateNotice = McMasterCreateNotice 


