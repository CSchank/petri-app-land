module TestNet.Update where
import TestNet.Static.Types
import Static.List
import Utils.Utils
import Static.ServerTypes

-- function called when new client connects (do not delete)
clientConnect :: ClientID -> A -> (A, APlayer)
clientConnect clientID a =
    error "Please fill out clientConnect function for the TestNet net."

-- functions called when a client disconnects (do not delete)
clientDisconnectFromA :: ClientID -> A -> APlayer -> A
clientDisconnectFromA clientID a aPlayer =
    error "Please fill out the clientDisconnectFromA function for the TestNet net."

clientDisconnectFromB :: ClientID -> B -> BPlayer -> B
clientDisconnectFromB clientID b bPlayer =
    error "Please fill out the clientDisconnectFromB function for the TestNet net."

clientDisconnectFromC :: ClientID -> C -> CPlayer -> C
clientDisconnectFromC clientID c cPlayer =
    error "Please fill out the clientDisconnectFromC function for the TestNet net."


-- functions for each transition
updateAB ::  AB -> A -> B -> List APlayer -> (A, B, APlayer -> ABfromA)
updateAB  (AB n) a b lstA =
    let
        fromA :: APlayer -> ABfromA
        fromA pa = error "Please fill in function stub."


    in
        (a, b, fromA)

updateCA :: ClientID -> CA -> C -> A -> List CPlayer -> (C, A, CPlayer -> CAfromC)
updateCA clientId (CA n) c a lstC =
    let
        fromC :: CPlayer -> CAfromC
        fromC pc = error "Please fill in function stub."


    in
        (c, a, fromC)

updateABC :: Maybe ClientID -> ABC -> A -> B -> C -> List APlayer -> List BPlayer -> (A, B, C, APlayer -> ABCfromA, BPlayer -> ABCfromB)
updateABC mClientId (ABC n) a b c lstA lstB =
    let
        fromA :: APlayer -> ABCfromA
        fromA pa = error "Please fill in function stub."

        fromB :: BPlayer -> ABCfromB
        fromB pb = error "Please fill in function stub."


    in
        (a, b, c, fromA, fromB)


