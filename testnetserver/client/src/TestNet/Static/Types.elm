module TestNet.Static.Types exposing(..)

-- the types of all places in the net
-- place states
type A  =
      A Int {-n-}


type B  =
      B Int {-n-}


type C  =
      C Int {-n-}



-- union place type
type NetState  =
      SA A
    | SB B
    | SC C
-- server transition types
type OutgoingTransition  =
      TCA Int
    | TABC Int
type CA  =
      CA Int
type ABC  =
      ABC Int

-- outgoing server message types
type StartGameAB  =
      StartGameAB Int {-n-}
type StartGameCA  =
      StartGameCA Int {-n-}
type StartGameAB2  =
      StartGameAB2 Int {-n-}
type StartGameAC  =
      StartGameAC Int {-n-}
type StartGameBC  =
      StartGameBC Int {-n-}
type IncomingMessage  =
      MStartGameAB Int {-n-}
    | MStartGameCA Int {-n-}
    | MStartGameAB2 Int {-n-}
    | MStartGameAC Int {-n-}
    | MStartGameBC Int {-n-}

-- extra server types


