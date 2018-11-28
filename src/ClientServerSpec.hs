module ClientServerSpec where

import Types
import TypeHelpers
import Data.Map as M

--where to output generated files
outputDirectory = "../elm-fraction-drawer/"

--client states
start = cState "Start" 
    [
        edt ElmString "userfield" ""
    ,   edt ElmString "passfield" ""
    ]
submitLogin = msg "SubmitLogin" 
    [
        edt ElmString "username" ""
    ,   edt ElmString "password" ""
    ]
login = msg "Login" []
typeuser = msg "TypeUser" [edt ElmString "username" ""]
typepass = msg "TypePass" [edt ElmString "password" ""]

wait = cState "Wait" 
    [
        edt ElmString "username" ""
    ]

viewingLobbies = cState "ViewingLobbies"
    [
        edt ElmString "currentUser" ""
    ,   edt (ElmList $ edt (ElmType "Lobby") "lobby" "") "lobbies" ""
    ,   edt (ElmIntRange 0 1000000) "scrollPos" ""
    ]

lobby = ec "Lobby"
    [
        constructor "Lobby" 
            [
                edt (ElmIntRange 0 10000000) "lobbyId" ""
            ,   edt (ElmIntRange 0 99) "icon" ""
            ,   edt ElmString "name" ""
            ,   edt (ElmList (edt ElmString "player" "")) "players" ""
            ]
    ]

--viewing lobby messages
joinLobby = msg "JoinLobby" [edt (ElmIntRange 0 10000000) "lobbyId" ""]
tapJoin = msg "TapJoin" [edt (ElmIntRange 0 10000000) "tappedLobbyId" ""]
tapCreate = msg "TapCreate" []
tapScrollUp = msg "ScrollUp" []
tapScrollDn = msg "ScrollDown" []

inLobby = cState "InLobby"
    [edt frac "fraction" ""
    ,edt maybeColour "c1" ""
    ,edt maybeColour "c2" ""
    ,edt maybeColour "c3" ""
    ,edt maybeColour "c4" ""
    ,edt (ElmIntRange 0 3) "numReady" "the number of players that are currently ready"
    ,playerNum
    ,edt (ElmMaybe channel) "draggingChannel" ""
    ,edt ElmString "lobbyName" ""
    ,edt (ElmIntRange 0 1000) "iconId" ""
    ,edt ElmString "objectToDraw" ""
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
    

gameWon = cState "GameWon" 
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
    ]

-- game won messages
tapBackToLobby = msg "TapBackToLobby" []

maybeColour = ElmMaybe (edt colour "colour" "")
colour = (ElmType "Colour")
channel = edt (ElmType "Channel") "channel" ""
channelType = ec "Channel" [constructor "Red" [],constructor "Green" [],constructor "Blue" []]
frac = ElmType "Frac"
fracType = ec "Frac" 
    [
        constructor "Frac" [edt (ElmIntRange 0 1000) "numerator" "",edt (ElmIntRange 1 1000) "denominator" ""]
    ]

--InLobby messages
startDragging = msg "StartDragging" [edt (ElmType "Channel") "channel" ""]
stopDragging = msg "StopDragging" []
dragging = msg "Dragging" [edt (ElmType "Channel") "channel" "", edt (ElmPair x y) "position" ""]
tapReady = msg "TapReady" []
tapStart = msg "TapStart" []
x = edt (ElmFloatRange 0 0 0) "x" ""
y = edt (ElmFloatRange 0 0 0) "y" ""
incIcon = msg "IncIcon" []
dcrIcon = msg "DecIcon" []

--Playing messages
startPainting = msg "StartPainting" [xCoord, yCoord]
enterBox = msg "EnterBox" [xCoord, yCoord]
stopPainting = msg "StopPainting" []

-- server states
nobody = sState "Nobody" [listFrac]
listFrac = edt (ElmList $ edt frac "fraction" "") "listFrac" ""

one = sState "One" 
    [listFrac
    , edt ElmString "drawingWord" ""
    , edt colour "client1Colour" ""  
    , edt clientId "client1ID" ""
    ]
clientId = ElmType "ClientID"
two = sState "Two" 
    [listFrac
    , edt ElmString "drawingWord" ""
    , edt colour "client1Colour" ""  
    , edt clientId "client1ID" ""
    , edt colour "client2Colour" ""  
    , edt clientId "client2ID" ""
    ]

three = sState "Three" 
    [listFrac
    , edt ElmString "drawingWord" ""
    , edt colour "client1Colour" ""  
    , edt clientId "client1ID" ""
    , edt colour "client2Colour" ""  
    , edt clientId "client2ID" ""
    , edt colour "client3Colour" ""  
    , edt clientId "client3ID" ""
    ]

four = sState "Four" 
    [listFrac
    , edt ElmString "drawingWord" ""
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
    , edt ElmString "drawingWord" ""
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

gameState = ec "GameState"
    [one,two,three,four,playingS]

serverState = sState "Idle"
    [
        edt (ElmDict (edt ElmString "username" "") (edt ElmString "password" "")) "userAccounts" ""
    ,   edt (ElmDict (edt clientId "cid" "") (edt ElmString "username" "")) "usersLoggedIn" ""
    ,   edt (ElmDict (edt clientId "cid" "") (edt (ElmIntRange 0 10000000) "gameId" "")) "user2GameId" ""
    ,   edt (ElmDict (edt (ElmIntRange 0 10000000) "gameId" "") gameRecord) "gameDict" ""
    ,   edt (ElmIntRange 0 1000000) "nextGid" ""
    ]

gameRecord = edt (ElmPair (edt (ElmIntRange 0 1000) "iconId" "") (edt (ElmType "GameState") "gameState" "")) "gameRecord" ""

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
startMsg = msg "SendStart" []
createLobbyMsg = msg "CreateNewLobby" []
changeIconMsg = msg "ChangeIcon" [edt (ElmIntRange 0 1000) "iconId" ""]

requestBackToLobby = msg "ReqBackToLobby" []
--server outgoing messages
sendFrac = msg "SendFrac" 
    [edt frac "newFrac" ""
    ,edt (ElmIntRange 0 3) "newPlayer" ""
    ,edt maybeColour "newP1C" ""
    ,edt maybeColour "newP2C" ""
    ,edt maybeColour "newP3C" ""
    ,edt maybeColour "newP4C" ""
    ]

couldNotJoin = msg "CouldNotJoin" []

acknowledgeJoin = msg "AcknowledgeJoinLobby" 
    [edt frac "newFrac" ""
    ,edt (ElmIntRange 0 3) "newPlayer" ""
    ,edt maybeColour "newP1C" ""
    ,edt maybeColour "newP2C" ""
    ,edt maybeColour "newP3C" ""
    ,edt maybeColour "newP4C" ""
    ,edt (ElmIntRange 0 99) "icon" ""
    ,edt ElmString "lobbyName" ""
    ,edt ElmString "drawingWord" ""
    ]

refreshLobby = msg "RefreshLobby" 
    [edt maybeColour "newP1C" ""
    ,edt maybeColour "newP2C" ""
    ,edt maybeColour "newP3C" ""
    ,edt maybeColour "newP4C" ""
    ,edt (ElmIntRange 0 99) "icon" ""
    ,edt ElmString "lobbyName" ""
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
incorrectLogin = msg "IncorrectLogin" []
sendLobbies = msg "SendLobbies"
    [
        edt (ElmList $ edt (ElmType "Lobby") "lobby" "") "lobbies" ""
    ]
sendGameWin = msg "SendGameWin" []
updateIcon = msg "UpdateIcon" [edt (ElmIntRange 0 1000) "newIcon" ""]

-- the actual app that is to be generated
clientServerApp :: ClientServerApp
clientServerApp = (
                ("Start", Just initTime) --client start state
             ,  "Idle" --server start start
             ,  [start,wait,viewingLobbies,inLobby,ready,playing,gameWon] --client states
             ,  [serverState]                                 --server states
             ,  [channelType,testRGB,fracType,lobby]                            --extra client types
             ,  [testRGB,fracType,gameState,lobby]        --extra server types
             ,  csDiagram --client state diagram
             ,  ssDiagram --server state diagram
             )

clientConnect = ("ClientConnect",[])
clientDisconnect = ("ClientDisconnect",[])

csDiagram :: ClientStateDiagram
csDiagram = M.fromList
            [
                (("Start",    login)            ,("Wait",       Nothing,Just submitLogin))
            ,   (("Start",    typeuser)         ,("Start",      Nothing,Nothing))
            ,   (("Start",    typepass)         ,("Start",      Nothing,Nothing))
            ,   (("Start",    initTime)         ,("Start",      Nothing,Nothing))
            ,   (("Wait",     incorrectLogin)   ,("Start",   Nothing,Nothing))
            ,   (("Wait",     sendLobbies)   ,("ViewingLobbies",   Nothing,Nothing))
            ,   (("ViewingLobbies", tapJoin)    ,("ViewingLobbies",   Nothing, Just joinLobby))
            ,   (("ViewingLobbies", couldNotJoin),("ViewingLobbies",   Nothing, Nothing))
            ,   (("ViewingLobbies", acknowledgeJoin),("InLobby",   Nothing, Nothing))
            ,   (("ViewingLobbies", sendLobbies),("ViewingLobbies",   Nothing, Nothing))
            ,   (("ViewingLobbies", tapScrollUp),("ViewingLobbies",   Nothing, Nothing))
            ,   (("ViewingLobbies", tapScrollDn),("ViewingLobbies",   Nothing, Nothing))
            ,   (("ViewingLobbies", tapCreate),("ViewingLobbies",   Nothing, Just createLobbyMsg))
            ,   (("InLobby", startDragging)    ,("InLobby",   Nothing,Nothing))
            ,   (("InLobby", stopDragging)     ,("InLobby",   Nothing,Nothing))
            ,   (("InLobby", dragging)         ,("InLobby",   Nothing,Just changeColour))
            ,   (("InLobby", refreshLobby)     ,("InLobby",   Nothing,Nothing))
            ,   (("InLobby", updateIcon)          ,("InLobby",   Nothing,Nothing))
            ,   (("InLobby", tapReady)         ,("InLobby",   Nothing,Just readyMsg))
            ,   (("InLobby", tapStart)         ,("InLobby",   Nothing,Just startMsg))
            ,   (("InLobby", readyToStart)     ,("InLobby",   Nothing,Nothing))
            ,   (("InLobby", moreReady)        ,("InLobby",   Nothing,Nothing))
            ,   (("InLobby", incIcon)    ,("InLobby",   Nothing,Just changeIconMsg))
            ,   (("InLobby", dcrIcon)    ,("InLobby",   Nothing,Just changeIconMsg))
            ,   (("InLobby", acknowledgeReady) ,("Ready",      Nothing,Nothing))
            ,   (("InLobby", startGame)        ,("Playing",    Nothing,Nothing))
            ,   (("Ready", startGame)          ,("Playing",    Nothing,Nothing))
            ,   (("Ready", moreReady)          ,("Ready",    Nothing,Nothing))
            ,   (("Playing", startPainting)    ,("Playing",    Nothing,Just tapMsg))
            ,   (("Playing", enterBox)         ,("Playing",    Nothing,Just tapMsg))
            ,   (("Playing", stopPainting)     ,("Playing",    Nothing,Nothing))
            ,   (("Playing", sendTap)          ,("Playing",    Nothing,Nothing))
            ,   (("Playing", sendGameWin)      ,("GameWon",    Nothing,Nothing))
            ,   (("GameWon", tapBackToLobby)   ,("GameWon",    Nothing,Just requestBackToLobby))
            ,   (("GameWon", sendLobbies)      ,("ViewingLobbies",    Nothing,Nothing))
            ]

ssDiagram :: ServerStateDiagram
ssDiagram = M.fromList 
            [
                (("Idle", clientConnect), ("Idle", NoClientMessage))
            ,   (("Idle", clientDisconnect), ("Idle",NoClientMessage))
            ,   (("Idle", submitLogin), ("Idle", OneOf [ToSender incorrectLogin, ToSender sendLobbies]))
            ,   (("Idle", joinLobby), ("Idle", OneOf [AllOf [ToSender acknowledgeJoin, ToSet sendLobbies, ToSet refreshLobby], ToSender couldNotJoin]))
            ,   (("Idle", changeColour), ("Idle", ToSet refreshLobby))
            ,   (("Idle", readyMsg), ("Idle", OneOf [AllOf [ToSet moreReady, ToSender acknowledgeReady], ToSender readyToStart]))
            ,   (("Idle", startMsg), ("Idle", ToSet startGame))
            ,   (("Idle", tapMsg), ("Idle", OneOf [ToSet sendTap, ToSet sendGameWin]))
            ,   (("Idle", requestBackToLobby), ("Idle", ToSender sendLobbies))
            ,   (("Idle", createLobbyMsg), ("Idle", AllOf [ToSender acknowledgeJoin, ToSet sendLobbies]))
            ,   (("Idle", changeIconMsg), ("Idle", AllOf [ToSet updateIcon, ToSet sendLobbies]))


            {- (("Nobody", submitLogin), ("One", ToSet sendFrac))
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
            ,   (("PlayingS", tapMsg), ("PlayingS", ToSet sendTap))-}
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