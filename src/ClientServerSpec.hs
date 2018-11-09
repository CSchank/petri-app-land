module ClientServerSpec where

import Types
import TypeHelpers
import Data.Map as M

--where to output generated files
outputDirectory = "../elm-fraction-drawer/"

--client states
start = cState "Start" []
submitLogin = msg "SubmitLogin" []
login = msg "Login" []

wait = cState "Wait" []


haveFrac = cState "HaveFrac" 
    [edt frac "fraction" ""
    ,edt maybeColour "c1" ""
    ,edt maybeColour "c2" ""
    ,edt maybeColour "c3" ""
    ,edt maybeColour "c4" ""
    ,edt (ElmIntRange 0 3) "numReady" "the number of players that are currently ready"
    ,playerNum
    ,edt (ElmMaybe channel) "draggingChannel" ""
    ]
ready = cState "Ready" 
    [edt frac "fraction" ""
    ,edt colour "c1" ""
    ,edt colour "c2" ""
    ,edt colour "c3" ""
    ,edt colour "c4" ""
    ,edt (ElmIntRange 1 3) "numReady" "the number of players that are currently ready"
    ,playerNum
    ]
playing = cState "Playing" 
    [edt frac "fraction" ""
    ,edt colour "c1" ""
    ,edt colour "c2" ""
    ,edt colour "c3" ""
    ,edt colour "c4" ""
    ,edt (ElmIntRange 0 3) "playerNum" ""
    ,edt (ElmDict (edt (ElmPair xCoord yCoord) "key" "coordinate") 
                  (edt colour "c" "box colour")
         )
         "grid" ""
    ,edt ElmBool "painting" "Is the user painting currently?"
    ]
playerNum = edt (ElmIntRange 0 3) "playerNum" ""
maxY = 19
maxX = 19
xCoord = edt (ElmIntRange 0 maxX) "x" ""
yCoord = edt (ElmIntRange 0 maxY) "y" ""
    

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
tapReady = msg "TapReady" []
tapStart = msg "TapStart" []
x = edt (ElmFloatRange 0 0 0) "x" ""
y = edt (ElmFloatRange 0 0 0) "y" ""

--Playing messages
startPainting = msg "StartPainting" [xCoord, yCoord]
enterBox = msg "EnterBox" [xCoord, yCoord]
stopPainting = msg "StopPainting" []

-- server states
nobody = sState "Nobody" [listFrac]
listFrac = edt (ElmList $ edt frac "fraction" "") "listFrac" ""

one = sState "One" 
    [listFrac
    , edt colour "client1Colour" ""  
    , edt clientId "client1ID" ""
    ]
clientId = ElmType "ClientID"
two = sState "Two" 
    [listFrac
    , edt colour "client1Colour" ""  
    , edt clientId "client1ID" ""
    , edt colour "client2Colour" ""  
    , edt clientId "client2ID" ""
    ]

three = sState "Three" 
    [listFrac
    , edt colour "client1Colour" ""  
    , edt clientId "client1ID" ""
    , edt colour "client2Colour" ""  
    , edt clientId "client2ID" ""
    , edt colour "client3Colour" ""  
    , edt clientId "client3ID" ""
    ]

four = sState "Four" 
    [listFrac
    , edt colour "client1Colour" ""  
    , edt clientId "client1ID" ""
    , edt colour "client2Colour" ""  
    , edt clientId "client2ID" ""
    , edt colour "client3Colour" ""  
    , edt clientId "client3ID" ""
    , edt colour "client4Colour" ""  
    , edt clientId "client4ID" ""
    , edt ElmBool "client1Ready" ""
    , edt ElmBool "client2Ready" ""
    , edt ElmBool "client3Ready" ""
    , edt ElmBool "client4Ready" ""
    ]

playingS = sState "PlayingS" 
    [listFrac
    , edt colour "client1Colour" ""  
    , edt clientId "client1ID" ""
    , edt colour "client2Colour" ""  
    , edt clientId "client2ID" ""
    , edt colour "client3Colour" ""  
    , edt clientId "client3ID" ""
    , edt colour "client4Colour" ""  
    , edt clientId "client4ID" ""
    , edt (ElmDict (edt (ElmPair xCoord yCoord) "key" "coordinate") 
                   (edt (ElmIntRange 0 3) "boxOwner" "last player who tapped on box"))
          "grid"
          "records who last tapped on boxes"
    ]

--client outgoing messages
changeColour = msg "ChangeColour" 
    [
        edt colour "colour" ""
    ]

readyMsg = msg "SendReady"
    [
    ]
tapMsg = msg "Tap"
    [
        xCoord
    ,   yCoord
    ]
startMsg = msg "SendStart"
    [
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
sendTap = msg "SendTap"
    [otherPlayer,xCoord,yCoord]
otherPlayer = edt (ElmIntRange 0 3) "otherPlayer" "number of other player who tapped"
startGame = msg "StartGame" []
moreReady = msg "MoreReady" 
    [
        edt (ElmIntRange 0 3) "playerIdReady" ""
    ]
readyToStart = msg "ReadyToStart" []
acknowledgeReady = msg "AcknowledgeReady" 
    [
        edt (ElmIntRange 1 4) "numPlayersReady" ""
    ]
initTime = msg "InitTime" [edt (ElmIntRange 0 0) "time" ""]

-- the actual app that is to be generated
clientServerApp :: ClientServerApp
clientServerApp = (
                ("Start", Just initTime) --client start state
             ,  "Nobody" --server start start
             ,  [start,wait,haveFrac,ready,playing] --client states
             ,  [nobody,one,two,three,four,playingS]                                 --server states
             ,  [channelType,testRGB,fracType]                            --extra client types
             ,  [testRGB,fracType]        --extra server types
             ,  csDiagram --client state diagram
             ,  ssDiagram --server state diagram
             )

clientConnect = ("ClientConnect",[])
clientDisconnect = ("ClientDisconnect",[])

csDiagram :: ClientStateDiagram
csDiagram = M.fromList 
            [
                (("Start",    login)            ,("Wait",      Nothing,Just submitLogin))
            ,   (("Start",    initTime)         ,("Start",      Nothing,Nothing))
            ,   (("Wait",     sendFrac)         ,("HaveFrac",   Nothing,Nothing))
            ,   (("HaveFrac", startDragging)    ,("HaveFrac",   Nothing,Nothing))
            ,   (("HaveFrac", stopDragging)     ,("HaveFrac",   Nothing,Nothing))
            ,   (("HaveFrac", dragging)         ,("HaveFrac",   Nothing,Just changeColour))
            ,   (("HaveFrac", sendNewColours)   ,("HaveFrac",   Nothing,Nothing))
            ,   (("HaveFrac", sendFrac)         ,("HaveFrac",   Nothing,Nothing))
            ,   (("HaveFrac", tapReady)         ,("HaveFrac",   Nothing,Just readyMsg))
            ,   (("HaveFrac", tapStart)         ,("HaveFrac",   Nothing,Just startMsg))
            ,   (("HaveFrac", readyToStart)     ,("HaveFrac",   Nothing,Nothing))
            ,   (("HaveFrac", moreReady)        ,("HaveFrac",   Nothing,Nothing))
            ,   (("HaveFrac", acknowledgeReady) ,("Ready",      Nothing,Nothing))
            ,   (("HaveFrac", startGame)        ,("Playing",    Nothing,Nothing))
            ,   (("Ready", startGame)           ,("Playing",    Nothing,Nothing))
            ,   (("Ready", moreReady)           ,("Ready",    Nothing,Nothing))
            ,   (("Playing", startPainting)     ,("Playing",    Nothing,Just tapMsg))
            ,   (("Playing", enterBox)          ,("Playing",    Nothing,Just tapMsg))
            ,   (("Playing", stopPainting)      ,("Playing",    Nothing,Nothing))
            ,   (("Playing", sendTap)           ,("Playing",    Nothing,Nothing))
            ]

ssDiagram :: ServerStateDiagram
ssDiagram = M.fromList 
            [
                (("Nobody", submitLogin), ("One", ToSet sendFrac))
            ,   (("Nobody", clientConnect), ("Nobody", NoClientMessage))
            ,   (("Nobody", clientDisconnect), ("Nobody", NoClientMessage))
            ,   (("One", submitLogin), ("Two", ToSet sendFrac))
            ,   (("One", changeColour), ("One", ToSet sendNewColours))
            ,   (("One", clientConnect), ("One", NoClientMessage))
            ,   (("One", clientDisconnect), ("Nobody", NoClientMessage))
            ,   (("Two", submitLogin), ("Three", ToSet sendFrac))
            ,   (("Two", changeColour), ("Two", ToSet sendNewColours))
            ,   (("Two", clientConnect), ("Two", NoClientMessage))
            ,   (("Two", clientDisconnect), ("One", ToSet sendFrac))
            ,   (("Three", submitLogin), ("Four", ToSet sendFrac))
            ,   (("Three", changeColour), ("Three", ToSet sendNewColours))
            ,   (("Three", clientConnect), ("Three", NoClientMessage))
            ,   (("Three", clientDisconnect), ("Two", ToSet sendFrac))
            ,   (("Four", submitLogin), ("Four", NoClientMessage))
            ,   (("Four", changeColour), ("Four", ToSet sendNewColours))
            ,   (("Four", clientConnect), ("Four", NoClientMessage))
            ,   (("Four", clientDisconnect), ("Three", ToSet sendFrac))
            ,   (("Four", readyMsg), ("Four", OneOf [AllOf [ToSet moreReady, ToSender acknowledgeReady], ToSender readyToStart]))
            ,   (("Four", startMsg), ("PlayingS", ToSet startGame))
            ,   (("PlayingS", clientConnect), ("PlayingS", NoClientMessage))
            ,   (("PlayingS", clientDisconnect), ("PlayingS", NoClientMessage))
            ,   (("PlayingS", tapMsg), ("PlayingS", ToSet sendTap))
            ]

testRGB :: ElmCustom
testRGB = ElmCustom "Colour" [("RGB", 
                                    [ (ElmIntRange 0 255, "red", "defines the red value of the RGB colour")
                                    , (ElmIntRange 0 255, "green", "defines the green value of the RGB colour")
                                    , (ElmIntRange 0 255, "blue", "defines the blue value of the RGB colour")
                                     ]
                              )
                          , ("HSL", 
                                    [ (ElmFloatRange 0 1 7, "hue", "defines the hue value of the HSL colour")
                                    , (ElmFloatRange 0 1 7, "saturation", "defines the saturation value of the HSL colour")
                                    , (ElmFloatRange 0 1 7, "light", "defines the light value of the HSL colour")
                                     ]
                              )
                          , ("RGBA", 
                                    [ (ElmIntRange 0 255, "red", "defines the red value of the RGBA colour")
                                    , (ElmIntRange 0 255, "green", "defines the green value of the RGBA colour")
                                    , (ElmIntRange 0 255, "blue", "defines the blue value of the RGBA colour")
                                    , (ElmFloatRange 0 1 7, "alpha", "defines the alpha value of the RGBA colour")
                                     ]
                              )
                          , ("HSLA", 
                                    [ (ElmFloatRange 0 1 7, "hue", "defines the hue value of the HSL colour")
                                    , (ElmFloatRange 0 1 7, "saturation", "defines the saturation value of the HSL colour")
                                    , (ElmFloatRange 0 1 7, "light", "defines the light value of the HSL colour")
                                    , (ElmFloatRange 0 1 7, "alpha", "defines the alpha value of the HSLA colour")
                                     ]
                              )
                          ]