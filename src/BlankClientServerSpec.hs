module BlankClientServerSpec where

import Types
import TypeHelpers
import Data.Map as M

--where to output generated files
outputDirectory = "../elm-number-race-new/"

--client states
login = cState "Login" []
--login messages
tapLogin = msg "TapLogin" []
sendLogin = msg "SendLogin" []


waiting = cState "Waiting"
    [
        edt (ElmIntRange 0 1000) "myPrime" "The prime number that this client will increment by."
    ]

--waiting messages
tapStart = msg "TapStart" []
sendStart = msg "SendStart" []

--server states

--data Idle =
--    Idle (List ClientID {-clientId-}) {-playerList-}

idle = sState "Idle" 
    [
        edt (ElmList (edt (ElmType "ClientID") "clientId" "The client id of the person")) "playerList" "The list of players who are logged in."
    ]

playingS = sState "PlayingS" 
    [
        edt (ElmList (edt (ElmType "ClientID") "clientId" "The client id of the person")) "playerList" "The list of players who are logged in."
    ,   edt (ElmIntRange 0 1000) "goal" ""
    ,   edt (ElmIntRange 0 1000) "currentNum" ""
    ]

--server messages
successfulLogin = msg "LoginSuccess" 
    [
       edt (ElmIntRange 0 1000) "yourNewPrime" ""
    ]

-- the actual app that is to be generated
clientServerApp :: ClientServerApp
clientServerApp = (
                ("Login", Nothing)  --client start state
             ,  ("Idle", Nothing)             --server start start
             ,  [login, waiting]                  --client states
             ,  [idle, playingS]                                 --server states
             ,  []                            --extra client types
             ,  []        --extra server types
             ,  csDiagram --client state diagram
             ,  ssDiagram --server state diagram
             ,  []
             )

clientConnect = ("ClientConnect",[])
clientDisconnect = ("ClientDisconnect",[])

csDiagram ::  M.Map (String, ClientTransition) (String, Maybe ClientCmd, Maybe ServerTransition)
csDiagram = M.fromList
            [
                (("Login", tapLogin), ("Login", Nothing, Just sendLogin))
                ,   (("Login", successfulLogin), ("Waiting", Nothing, Nothing))
                ,   (("Waiting", tapStart), ("Waiting", Nothing, Just sendStart))
            ]

ssDiagram :: ServerStateDiagram
ssDiagram = M.fromList 
            [
                (("Idle", clientConnect),       ("Idle", Nothing, NoClientMessage))
                ,   (("Idle", clientDisconnect),    ("Idle", Nothing, NoClientMessage))
                ,   (("PlayingS", clientConnect),    ("PlayingS", Nothing, NoClientMessage))
                ,   (("PlayingS", clientDisconnect),    ("PlayingS", Nothing, NoClientMessage))
                ,   (("Idle", sendLogin),    ("Idle", Nothing, ToSender successfulLogin))
            ]