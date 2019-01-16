module TestNet.Update where
import TestNet.Static.Types
import TestNet.Static.FromSuperPlace
import Static.List
import Utils.Utils
import Static.ServerTypes

-- function called when new client connects (do not delete)
clientConnect :: FromSuperPlace -> ClientID -> A -> (A, APlayer)
clientConnect fsp clientID a =
    error "Please fill out clientConnect function for the TestNet net."

-- functions called when a client disconnects (do not delete)
clientDisconnectFromA :: FromSuperPlace -> ClientID -> A -> APlayer -> A
clientDisconnectFromA fsp clientID a aPlayer =
    error "Please fill out the clientDisconnectFromA function for the TestNet net."

clientDisconnectFromB :: FromSuperPlace -> ClientID -> B -> BPlayer -> B
clientDisconnectFromB fsp clientID b bPlayer =
    error "Please fill out the clientDisconnectFromB function for the TestNet net."

clientDisconnectFromC :: FromSuperPlace -> ClientID -> C -> CPlayer -> C
clientDisconnectFromC fsp clientID c cPlayer =
    error "Please fill out the clientDisconnectFromC function for the TestNet net."


-- functions for each transition
updateAB :: FromSuperPlace ->  AB -> A -> B -> List APlayer -> (A, B, APlayer -> ABfromA)
updateAB fsp  (AB n) a b lstA =
    let
        fromA :: APlayer -> ABfromA
        fromA pa = error "Please fill in function stub."


    in
        (a, b, fromA)

updateCA :: FromSuperPlace -> ClientID -> CA -> C -> A -> List CPlayer -> (C, A, CPlayer -> CAfromC)
updateCA fsp clientId (CA n) c a lstC =
    let
        fromC :: CPlayer -> CAfromC
        fromC pc = error "Please fill in function stub."


    in
        (c, a, fromC)

updateABC :: FromSuperPlace -> Maybe ClientID -> ABC -> A -> B -> C -> List APlayer -> List BPlayer -> (A, B, C, APlayer -> ABCfromA, BPlayer -> ABCfromB)
updateABC fsp mClientId (ABC n) a b c lstA lstB =
    let
        fromA :: APlayer -> ABCfromA
        fromA pa = error "Please fill in function stub."

        fromB :: BPlayer -> ABCfromB
        fromB pb = error "Please fill in function stub."


    in
        (a, b, c, fromA, fromB)


