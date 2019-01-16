module TestNet.Static.Types exposing(..)

-- the types of all places in the net
-- place states
data A  =
      A Int {-n-}
    deriving(Ord,Eq,Show,Typeable)


data B  =
      B Int {-n-}
    deriving(Ord,Eq,Show,Typeable)


data C  =
      C Int {-n-}
    deriving(Ord,Eq,Show,Typeable)



-- union place type
type NetState  =
      SA A
    | SB B
    | SC C
-- outgoing server message types
data StartGameAB  =
      StartGameAB Int {-n-}
    deriving(Ord,Eq,Show)
data StartGameCA  =
      StartGameCA Int {-n-}
    deriving(Ord,Eq,Show)
data StartGameAB2  =
      StartGameAB2 Int {-n-}
    deriving(Ord,Eq,Show)
data StartGameAC  =
      StartGameAC Int {-n-}
    deriving(Ord,Eq,Show)
data StartGameBC  =
      StartGameBC Int {-n-}
    deriving(Ord,Eq,Show)
data IncomingMessage  =
      MStartGameAB Int {-n-}
    | MStartGameCA Int {-n-}
    | MStartGameAB2 Int {-n-}
    | MStartGameAC Int {-n-}
    | MStartGameBC Int {-n-}
    deriving(Ord,Eq,Show)

-- individual transition types
data ABfromA  =
      AB_AtoB BPlayer StartGameAB
    deriving(Ord,Eq,Show)

data CAfromC  =
      CA_CtoA APlayer StartGameCA
    deriving(Ord,Eq,Show)

data ABCfromA  =
      ABC_AtoC CPlayer StartGameAC
    | ABC_AtoB BPlayer StartGameAB2
    deriving(Ord,Eq,Show)
data ABCfromB  =
      ABC_BtoC CPlayer StartGameBC
    deriving(Ord,Eq,Show)


-- extra server types


-- the FromSuperPlace type
