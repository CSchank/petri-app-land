module TestFraction where

import Types
import qualified Data.Map as M
import Generate.Server 
import TestElmTypes
import Generate.OneOf
import Generate.Dot
import TypeHelpers


--client states
start = state "Start" []
submitLogin = msg "SubmitLogin" []
login = msg "Login" []

wait = state "Wait" []


haveFrac = state "HaveFrac" 
    [edt frac "fraction" ""
    ,edt maybeColour "c1" ""
    ,edt maybeColour "c2" ""
    ,edt maybeColour "c3" ""
    ,edt maybeColour "c4" ""
    ,edt (ElmIntRange 0 3) "playerNum" ""
    ,edt (ElmMaybe channel) "draggingChannel" ""
    ]
maybeColour = ElmMaybe (edt colour "colour" "")
colour = (ElmType "Colour")
channel = edt (ElmType "Channel") "channel" ""
channelType = ec "Channel" [constructor "Red" [],constructor "Green" [],constructor "Blue" []]
frac = ElmType "Frac"
fracType = ec "Frac" 
    [
        constructor "Frac" [edt (ElmIntRange 0 1000) "numerator" "",edt (ElmIntRange 1 1000) "denominator" ""]
    ]

--HaveFrac messages
startDragging = msg "StartDragging" [edt (ElmType "Channel") "channel" ""]
stopDragging = msg "StopDragging" []
dragging = msg "Dragging" [edt (ElmType "Channel") "channel" "", edt (ElmPair x y) "position" ""]
x = edt (ElmFloatRange 0 0 0) "x" ""
y = edt (ElmFloatRange 0 0 0) "y" ""

-- server states
nobody = state "Nobody" [listFrac]
listFrac = edt (ElmList $ edt frac "fraction" "") "listFrac" ""

one = state "One" 
    [listFrac
    , edt colour "client1Colour" ""  
    , edt clientId "client1ID" ""
    ]
clientId = ElmType "ClientID"
two = state "Two" 
    [listFrac
    , edt colour "client1Colour" ""  
    , edt clientId "client1ID" ""
    , edt colour "client2Colour" ""  
    , edt clientId "client2ID" ""
    ]

three = state "Three" 
    [listFrac
    , edt colour "client1Colour" ""  
    , edt clientId "client1ID" ""
    , edt colour "client2Colour" ""  
    , edt clientId "client2ID" ""
    , edt colour "client3Colour" ""  
    , edt clientId "client3ID" ""
    ]

four = state "Four" 
    [listFrac
    , edt colour "client1Colour" ""  
    , edt clientId "client1ID" ""
    , edt colour "client2Colour" ""  
    , edt clientId "client2ID" ""
    , edt colour "client3Colour" ""  
    , edt clientId "client3ID" ""
    , edt colour "client4Colour" ""  
    , edt clientId "client4ID" ""
    ]

--client outgoing messages
changeColour = msg "ChangeColour" 
    [
        edt colour "colour" ""
    ]

--server outgoing messages
sendFrac = msg "SendFrac" 
    [edt frac "newFrac" ""
    ,edt (ElmIntRange 0 3) "newPlayer" ""
    ,edt maybeColour "newP1C" ""
    ,edt maybeColour "newP2C" ""
    ,edt maybeColour "newP3C" ""
    ,edt maybeColour "newP4C" ""
    ]

sendNewColours = msg "SendNewColours"
    [edt maybeColour "newP1C" ""
    ,edt maybeColour "newP2C" ""
    ,edt maybeColour "newP3C" ""
    ,edt maybeColour "newP4C" ""
    ]

testServer :: ClientServerApp
testServer = (
                "Start" --client start state
             ,  "Nobody" --server start start
             ,  M.fromList [("Start",start),("Wait",wait),("HaveFrac",haveFrac)] --client states
             ,  M.fromList [("Nobody",nobody),("One",one),("Two",two),("Three",three),("Four",four)]                                 --server states
             ,  M.fromList [("Channel",channelType),("Colour",testRGB),("Frac",fracType)]                            --extra client types
             ,  M.fromList [("Colour",testRGB),("Frac",fracType)]        --extra server types
             ,  csDiagram --client state diagram
             ,  ssDiagram --server state diagram
             )

clientConnect = ("ClientConnect",[])
clientDisconnect = ("ClientDisconnect",[])

csDiagram :: ClientStateDiagram
csDiagram = M.fromList 
            [
                (("Start",    login)     ,("Wait",    Just submitLogin))
            ,   (("Wait",     sendFrac)     ,("HaveFrac",   Nothing))
            ,   (("HaveFrac", startDragging)     ,("HaveFrac",   Nothing))
            ,   (("HaveFrac", stopDragging)     ,("HaveFrac",   Nothing))
            ,   (("HaveFrac", dragging)     ,("HaveFrac",   Just changeColour))
            ,   (("HaveFrac", sendNewColours)     ,("HaveFrac",   Nothing))
            ,   (("HaveFrac", sendFrac)     ,("HaveFrac",   Nothing))
            ]

ssDiagram :: ServerStateDiagram
ssDiagram = M.fromList 
            [
                (("Nobody", submitLogin), ("One", ToAll sendFrac))
            ,   (("Nobody", clientConnect), ("Nobody", NoClientMessage))
            ,   (("Nobody", clientDisconnect), ("Nobody", NoClientMessage))
            ,   (("One", submitLogin), ("Two", ToAll sendFrac))
            ,   (("One", changeColour), ("One", ToAll sendNewColours))
            ,   (("One", clientConnect), ("One", NoClientMessage))
            ,   (("One", clientDisconnect), ("Nobody", NoClientMessage))
            ,   (("Two", submitLogin), ("Three", ToAll sendFrac))
            ,   (("Two", changeColour), ("Two", ToAll sendNewColours))
            ,   (("Two", clientConnect), ("Two", NoClientMessage))
            ,   (("Two", clientDisconnect), ("One", ToAll sendFrac))
            ,   (("Three", submitLogin), ("Four", ToAll sendFrac))
            ,   (("Three", changeColour), ("Three", ToAll sendNewColours))
            ,   (("Three", clientConnect), ("Three", NoClientMessage))
            ,   (("Three", clientDisconnect), ("Two", ToAll sendFrac))
            ,   (("Four", submitLogin), ("Four", NoClientMessage))
            ,   (("Four", changeColour), ("Four", ToAll sendNewColours))
            ,   (("Four", clientConnect), ("Four", NoClientMessage))
            ,   (("Four", clientDisconnect), ("Three", ToAll sendFrac))
            ]