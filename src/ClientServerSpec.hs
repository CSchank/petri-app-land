module ClientServerSpec where

import Types
import TypeHelpers
import Data.Map as M
import Generate.Plugins.Database

--where to output generated files
outputDirectory = "../elm-number-race/"

--client states
login = cState "Login" []
--login messages
tapLogin = msg "TapLogin" []
sendLogin = msg "SendLogin" []

waiting = cState "Waiting" 
    [
        edt (ElmIntRange 0 1000) "clientN" "The number that this client will increment by"
    ]
--waiting messages
tapStart = msg "TapStart" []
sendStart = msg "SendStart" []
startGame = msg "StartGame" 
    [
        edt (ElmIntRange 0 1000) "goal" "The goal of the counter"
    ]

playingC = cState "InGame" 
    [
        edt (ElmIntRange 0 1000) "clientN" "The number that this client will increment by"
    ,   edt (ElmIntRange 0 1000) "currentNum" "The current state of the counter"
    ,   edt (ElmIntRange 0 1000) "goal" "The goal of the counter"
    ]
--client playing messages
tapIncrement = msg "TapIncrement" [] --Tell the server to increase the number
sendIncrement = msg "SendIncrement" [] --Tell the server to increase the number
newValue = msg "NewValue" 
    [
        edt (ElmIntRange 0 10000) "n" "The new value of the counter"
    ]
tapTryAgain = msg "TapTryAgain" [] --Tell the server to increase the number
sendTryAgain = msg "SendTryAgain" [] --Tell the server to increase the number



--server states
idle = sState "Idle"
    [
        edt (ElmDict clientId incrementN) "pIncs" "All players and their amount of incrementing"
    ]

incrementN = edt (ElmIntRange 0 1000) "n" "The number that this client will increment by"
clientId = edt (ElmType "ClientID") "clientID" ""


playingS = sState "PlayingS"
    [
        edt (ElmDict clientId incrementN) "pIncs" "All players and their amount of incrementing"
    ,   edt (ElmIntRange 0 1000) "currentNum" "The current state of the counter"
    ,   edt (ElmIntRange 0 1000) "goal" "The goal of the counter"
    ]

--server messages
loginSuccess = msg "LoginSuccess"
    [
        edt (ElmIntRange 0 1000) "clientN" "The number that this client will increment by"
    ]
randomGoal = msg "RandomGoal"
    [
        edt (ElmIntRange 0 1000) "randomGoal" ""
    ]

-- the actual app that is to be generated
clientServerApp :: ClientServerApp
clientServerApp = (
                ("Login", Nothing) --client start state
             ,  ("Idle", Nothing) --server start start
             ,  [login,waiting,playingC] --client states
             ,  [idle,playingS]                                 --server states
             ,  []                            --extra client types
             ,  []        --extra server types
             ,  csDiagram --client state diagram
             ,  ssDiagram --server state diagram
             ,  [ Plugin "Incrementer"
                --, PluginGen "Database" (generateDatabase [Table "People" [(ElmString,"name","")] []])
                ]
             )

clientConnect = ("ClientConnect",[])
clientDisconnect = ("ClientDisconnect",[])

csDiagram ::  M.Map (String, ClientTransition) (String, Maybe ClientCmd, Maybe ServerTransition)
csDiagram = M.fromList
            [
                (("Login",   tapLogin)            ,("Login",       Nothing, Just sendLogin))
            ,   (("Login",   loginSuccess)        ,("Waiting",        Nothing, Nothing))
            ,   (("Waiting",   tapStart)          ,("Waiting",        Nothing, Just sendStart))
            ,   (("Waiting",   startGame)         ,("InGame",        Nothing, Nothing))
            ,   (("InGame",   tapIncrement)       ,("InGame",        Nothing, Just sendIncrement))
            ,   (("InGame",   newValue)           ,("InGame",        Nothing, Nothing))
            ,   (("InGame",   tapTryAgain)        ,("InGame",        Nothing, Just sendTryAgain))
            ]

ssDiagram :: ServerStateDiagram
ssDiagram = M.fromList 
            [
                (("Idle", clientConnect),       ("Idle", Nothing, NoClientMessage))
            ,   (("Idle", clientDisconnect),    ("Idle", Nothing, NoClientMessage))
            ,   (("Idle", sendLogin),           ("Idle", Nothing, ToSender loginSuccess))
            ,   (("Idle", sendStart),           ("Idle", Just randomGoal, NoClientMessage))
            ,   (("Idle", randomGoal),          ("PlayingS", Nothing, ToSet startGame))
            ,   (("PlayingS", sendIncrement),    ("PlayingS", Nothing, ToSet newValue))
            ,   (("PlayingS", sendTryAgain),     ("PlayingS", Nothing, ToSet newValue))
            ]