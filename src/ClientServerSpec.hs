module ClientServerSpec where

import Types
import TypeHelpers
import Data.Map as M
import Utils

--where to output generated files
outputDirectory = "../new-youth-hack/ChatApp/"

--client states
start = cState "Login" 
    [
        edt ElmString "username" ""
    ,   edt ElmString "password" ""
    ] 
        |> withSub timeMsg
timeMsg = constructor "NewTime" [edt (ElmIntRange 0 100000000000) "time" ""]
-- login messages
user = msg "User" [edt ElmString "user" ""]
pass = msg "Pass" [edt ElmString "pass" ""]
tapLogin = msg "TapLogin" []

choosePath = cState "ChoosePath" []
-- choose path messages
chooseHighSchool = msg "HighSchool" []
choosePostSecondary = msg "PostSecondary" []

interestedPrograms = cState "InterestedPrograms" 
    [
        edt listInt "checkedPrograms" ""
    ]

--interested programs messages
checkProgram = msg "CheckProgram" [edt (ElmIntRange 0 10) "programId" ""]


listInt = ElmList (edt (ElmIntRange 0 10) "programId" "")
interestedSchools = cState "InterestedSchools" 
    [
        edt listInt "checkedSchools" ""
    ]
-- interested schools messages
checkSchool = msg "CheckSchool" [edt (ElmIntRange 0 10) "schoolId" ""]

writeBio = cState "WriteBio" 
    [
        edt ElmString "bio" ""
    ,   edt ElmBool "isPostSec" ""
    ]
-- write bio message
editBio = msg "EditBio" [edt ElmString "newBio" ""]

chooseProgram = cState "ChooseProgram" []

--choose program messages
changeProgram = msg "ChangeProgram" [edt ElmString "program" ""]
changeInst = msg "ChangeInst" [edt ElmString "institution" ""]

enterEmail = cState "EnterEmail"
    [
        edt ElmString "email" ""
    ]
-- enter email messages
typeEmail = msg "Email" [edt ElmString "newEmail" ""]

next = msg "Next" []

chatScreen = cState "ChatScreen"
    [
        edt ElmString "currentMessage" ""
    ,   edt chatMessages "messageList" ""
    ]
chatMessages = ElmList (edt (ElmTriple (edt ElmString "username" "") (edt ElmBool "isMentor" "") (edt ElmString "message" "")) "singleMessage" "")

--chat state messages
typeChatBox = msg "TypeChat"
    [
        edt ElmString "newMessage" ""
    ]
sendMessage = msg "ClickSend" []

--client outgoing messages
login = msg "LoginMessage" [edt ElmString "loginUsername" ""]
enterChat = msg "EnterChat" [edt ElmBool "isPostSec" ""]
newMessage = msg "NewMessage" [edt ElmString "outgoingChatMessage" ""]

--server states
clientId = ElmType "ClientID"

idle = sState "Idle" 
    [
        edt (ElmDict (edt clientId "clientId" "") (edt ElmString "username" "")) "clientInfo" ""
    ,   edt (ElmDict (edt clientId "clientId" "") (edt ElmBool "isPostSec" "")) "clientRecords" ""
    ]

--server outgoing messages
loginSuccess = msg "LoginSuccess" []

broadcastNewChat = msg "NewChatMessage"
    [
        edt ElmString "username" ""
    ,   edt ElmBool "isPostSec" ""
    ,   edt ElmString "message" ""
    ]


-- the actual app that is to be generated
clientServerApp :: ClientServerApp
clientServerApp = (
                   "Login" --client start state
                ,  "Idle" --server start start
                ,  [start,choosePath,interestedPrograms,interestedSchools,writeBio,chatScreen,chooseProgram,enterEmail] --client states
                ,  [idle]                                 --server states
                ,  []                            --extra client types
                ,  []        --extra server types
                ,  csDiagram --client state diagram
                ,  ssDiagram --server state diagram
                )

clientConnect = ("ClientConnect",[])
clientDisconnect = ("ClientDisconnect",[])

csDiagram :: ClientStateDiagram
csDiagram = M.fromList 
            [
                (("Login",    tapLogin)            ,("Login",    Just login))
            ,   (("Login",    user)                ,("Login",    Nothing))
            ,   (("Login",    pass)                ,("Login",    Nothing))
            ,   (("Login",    loginSuccess)        ,("ChoosePath",    Nothing))
            ,   (("Login",    timeMsg)        ,("Login",    Nothing))
            ,   (("ChoosePath",    chooseHighSchool)        ,("InterestedPrograms",    Nothing))
            ,   (("ChoosePath",    choosePostSecondary)        ,("ChooseProgram",    Nothing))
            ,   (("ChooseProgram",    next)        ,("EnterEmail",    Nothing))
            ,   (("InterestedPrograms",    checkProgram)        ,("InterestedPrograms",    Nothing))
            ,   (("InterestedPrograms",    next)        ,("InterestedSchools",    Nothing))
            ,   (("InterestedSchools",    checkSchool)        ,("InterestedSchools",    Nothing))
            ,   (("InterestedSchools",    next)        ,("WriteBio",    Nothing))
            ,   (("WriteBio",    editBio)        ,("WriteBio",    Nothing))
            ,   (("WriteBio",    next)        ,("ChatScreen",    Just enterChat))
            ,   (("ChatScreen",   typeChatBox)        ,("ChatScreen",    Nothing))
            ,   (("ChatScreen",   sendMessage)        ,("ChatScreen",    Just newMessage))
            ,   (("ChatScreen",   broadcastNewChat)        ,("ChatScreen",    Nothing))

            ,   (("ChooseProgram", next), ("EnterEmail", Nothing))
            ,   (("ChooseProgram", changeProgram), ("ChooseProgram", Nothing))
            ,   (("ChooseProgram", changeInst), ("ChooseProgram", Nothing))
            ,   (("EnterEmail", next), ("WriteBio", Nothing))
            ,   (("EnterEmail", typeEmail), ("EnterEmail", Nothing))
            ]

ssDiagram :: ServerStateDiagram
ssDiagram = M.fromList 
            [
                (("Idle",    login)            ,("Idle",    ToSender loginSuccess))
            ,   (("Idle",    enterChat)            ,("Idle",    NoClientMessage))
            ,   (("Idle",    newMessage)            ,("Idle",    ToAll broadcastNewChat))
            ,   (("Idle", clientConnect), ("Idle", NoClientMessage))
            ,   (("Idle", clientDisconnect), ("Idle", NoClientMessage))
            ]

