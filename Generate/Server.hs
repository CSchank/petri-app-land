{-# LANGUAGE OverloadedStrings #-}

module Generate.Server where

import                  Generate.Codec
import                  Generate.Types
import qualified        Data.Map                as M
import qualified        Data.Set                as S
import qualified        Data.Text               as T
import qualified        Data.Text.IO            as TIO
import                  Types
import                  Generate.Types
import                  System.Directory
import                  System.FilePath.Posix   ((</>),(<.>))
import                  Data.Maybe              (mapMaybe)
import                  Data.Time               (getCurrentTime)
import                  ServerTemplate.Utils
import                  ServerTemplate.ServerLogic
import                  ServerTemplate.Lib
import                  ServerTemplate.ServerTypes
import                  ServerTemplate.Main

import                  ClientTemplate.Utils
import                  ClientTemplate.ClientLogic
import                  ClientTemplate.Lib
import                  ClientTemplate.ClientTypes
import                  ClientTemplate.Main
import                  ClientTemplate.ElmJson
import                  ClientTemplate.View
import                  ClientTemplate.Version

cm2maybe :: OutgoingClientMessage -> Maybe OutgoingClientMessage
cm2maybe NoClientMessage       = Nothing
cm2maybe a = Just a

cm2constr :: OutgoingClientMessage -> Maybe ClientTransition
cm2constr NoClientMessage       = Nothing
cm2constr (OnlySender ct)       = Just ct
cm2constr (AllExceptSender ct)  = Just ct
cm2constr (SenderAnd ct)        = Just ct
cm2constr (ToAll ct)            = Just ct

generateServer :: Bool -> FilePath -> ClientServerApp -> IO ()
generateServer onlyStatic fp (startCs
                  ,startSs
                  ,cStates
                  ,sStates
                  ,cExtraT
                  ,sExtraT
                  ,cDiagram
                  ,sDiagram) = 
    let
        disclaimer date = T.unlines ["{-"
                                ,T.concat["    THIS FILE WAS AUTOMATICALLY GENERATED AT ", T.pack $ show date,"."]
                                , "    IMPORTANT: USE THIS FILE FOR REFERENCE ONLY. YOU SHOULD NOT MODIFY THIS FILE. INSTEAD, MODIFY THE STATE DIAGRAM AND REGENERATE THIS FILE."
                                , "    MODIFYING ANY FILES INSIDE THE static DIRECTORY COULD LEAD TO UNEXPECTED ERRORS IN YOUR APP."
                                ,"-}"
                                ]
        serverStateTypeTxt = 
            generateType True True $ ElmCustom "Model" $ map (\(n,t) -> ("S"++n,t)) $ M.elems sStates
        serverMsgs = S.toList $ serverTransFromClient `S.union` serverTransitions
        serverMsgType = ElmCustom "ServerMessage" $ map (\(n,t) -> ("M"++n,t)) serverMsgs
        serverMsgTypeTxt = generateType True True serverMsgType

        serverTransFromClient = S.fromList $ mapMaybe (\(_,(_,trans)) -> trans) $ M.toList cDiagram
        serverTransitions = S.fromList $ map (\((_,trans),_) -> trans) $ M.toList sDiagram

        serverOutgoingMsgs = S.toList $ S.fromList $ mapMaybe (\(_,(_,trans)) -> cm2maybe trans) $ M.toList sDiagram
        serverOutgoingMsgType = ElmCustom "ClientMessage" $ map (\(n,t) -> ("M"++n,t)) $ mapMaybe cm2constr serverOutgoingMsgs
        serverOutgoingMsgTypeTxt = generateType True True serverOutgoingMsgType
        
        clientStateTypeTxt = 
            generateType False True $ ElmCustom "Model" $ map (\(n,t) -> ("S"++n,t)) $ M.elems cStates

        clientMsgs = S.toList $ S.fromList $ map (\((_,trans),(_,_)) -> trans) $ M.toList cDiagram
        clientMsgType = ElmCustom "ClientMessage" $ map (\(n,t) -> ("M"++n,t)) clientMsgs
        clientMsgTypeTxt = generateType False True clientMsgType

        clientOutgoingMsgs = S.toList $ S.fromList $ mapMaybe (\(_,(_,trans)) -> trans) $ M.toList cDiagram
        clientOutgoingMsgType = ElmCustom "ServerMessage" $ map (\(n,t) -> ("M"++n,t)) clientOutgoingMsgs
        clientOutgoingMsgTypeTxt = generateType False True clientOutgoingMsgType

        serverWrappedMessagesTxt = T.unlines $ map (\(name,constrs) -> generateType True True $ ElmCustom name [(name,constrs)]) serverMsgs
        serverWrappedStateTypesTxt = T.unlines $ map (\(name,constrs) -> generateType True True $ ElmCustom name [(name,constrs)]) $ M.elems sStates
        serverWrappedOutgoingMsgTxt = T.unlines $ map (\(name,constrs) -> generateType True True $ ElmCustom name [(name,constrs)]) $ mapMaybe cm2constr serverOutgoingMsgs

        serverExtraTypes = map snd $ M.toList sExtraT
        serverExtraTypesTxt = T.unlines $ map (generateType True True) serverExtraTypes
        
        clientExtraTypes = map snd $ M.toList cExtraT
        clientExtraTypesTxt = T.unlines $ map (generateType False True) clientExtraTypes

        clientWrappedMessagesTxt = T.unlines $ map (\(name,constrs) -> generateType False True $ ElmCustom name [(name,constrs)]) clientMsgs
        clientWrappedStateTypesTxt = T.unlines $ map (\(name,constrs) -> generateType False True $ ElmCustom name [(name,constrs)]) $ M.elems cStates
        clientWrappedOutgoingMsgTxt = T.unlines $ map (\(name,constrs) -> generateType False True $ ElmCustom name [(name,constrs)]) clientOutgoingMsgs


        typesHs = 
                [
                 "{-"
                ,"    Looking for a place to define your own algebraic data types for internal use? Look at you becoming a real Haskell programmer! What a great idea."
                ,"    However, instead of defining them here, why not define them in userApp/Types.hs instead? We've conveniently created it for you :)"
                ,"-}\n"
                ,"module Static.Types where\n"
                ,"-- Server states"
                ,serverStateTypeTxt, ""
                ,"-- Server transitions"
                ,serverMsgTypeTxt,""
                ,"-- Outgoing messages"
                ,serverOutgoingMsgTypeTxt,""
                ,"-- Extra internal types"
                ,serverExtraTypesTxt,""
                ,"-- Wrapped server transitions"
                ,serverWrappedMessagesTxt
                ,"-- Wrapped states"
                ,serverWrappedStateTypesTxt
                ,"-- Wrapped outgoing messages"
                ,serverWrappedOutgoingMsgTxt
                ]

        typesElm = 
                [
                "{-"
                ,"    Looking for a place to define your own algebraic data types for internal use? Look at you becoming a real Elm programmer! What a great idea."
                ,"    However, instead of defining them here, why not define them in userApp/Types.hs instead? We've conveniently created it for you :)"
                ,"-}\n"
                ,"module Static.Types exposing(..)\n"
                ,"-- Client states"
                ,clientStateTypeTxt, ""
                ,"-- Client transitions"
                ,clientMsgTypeTxt,""
                ,"-- Outgoing Messages"
                ,clientOutgoingMsgTypeTxt,""
                ,"-- Extra internal types"
                ,clientExtraTypesTxt,""
                ,"-- Wrapped client transitions"
                ,clientWrappedMessagesTxt
                ,"-- Wrapped state types"
                ,clientWrappedStateTypesTxt
                ,"-- Wrapped outgoing messages"
                ,clientWrappedOutgoingMsgTxt
                ]        

        {-Encoders-}
        serverOutgoingEncoder = generateEncoder True $ ElmCustom "ClientMessage" $ map (\(n,t) -> ("M"++n,t)) $ mapMaybe cm2constr serverOutgoingMsgs
        serverExtraTypesEncoder = T.unlines $ map (generateEncoder True) serverExtraTypes
        encoderHs = ["{-# LANGUAGE OverloadedStrings #-}\n"
                    ,"module Static.Encode where\n"
                    ,"import Static.Types"
                    ,"import Utils.Utils"
                    ,"import qualified Data.Text as T"
                    ,serverOutgoingEncoder
                    ,serverExtraTypesEncoder]

        clientEncoder = generateEncoder False serverMsgType
        clientOutgoingEncoder = generateEncoder False $ ElmCustom "ServerMessage" $ map (\(n,t) -> ("M"++n,t)) clientOutgoingMsgs
        clientExtraTypesEncoder = T.unlines $ map (generateEncoder False) clientExtraTypes
        encoderElm = ["module Static.Encode exposing (..)\n"
                    ,"import Static.Types exposing (..)"
                    ,"import Utils.Utils exposing (..)"
                    ,clientOutgoingEncoder
                    ,clientExtraTypesEncoder]

        {-Decoders-}
        serverDecoder = generateDecoder True serverMsgType
        serverExtraTypesDecoder = T.unlines $ map (generateDecoder True) serverExtraTypes
        decoderHs = ["{-# LANGUAGE OverloadedStrings #-}\n"
                    ,"module Static.Decode where\n"
                    ,"import Utils.Utils"
                    ,"import Data.Text as T"
                    ,"import Static.Types\n"
                    ,serverDecoder
                    ,serverExtraTypesDecoder]

        clientDecoder = generateDecoder False clientMsgType
        clientExtraTypesDecoder = T.unlines $ map (generateDecoder False) clientExtraTypes
        decoderElm = ["module Static.Decode exposing (..)\n"
                    ,"import Utils.Utils exposing (..)"
                    ,"import String"
                    ,"import Static.Types exposing(..)\n"
                    ,clientDecoder
                    ,clientExtraTypesDecoder]

        createServerUserUpdate :: ((String, ServerTransition), (String, OutgoingClientMessage)) -> T.Text
        createServerUserUpdate ((s0,(tn,tetd)),(s1,mCt)) =
            let
                (.::.) = " :: "
                (_,s0Cons) = case M.lookup s0 sStates of
                            Just constr -> constr
                            Nothing -> error $ "State " ++ s0 ++ " does not exist in the map."
                s1Cons = case M.lookup s1 sStates of
                            Just constr -> constr
                            Nothing -> error $ "State " ++ s0 ++ " does not exist in the map."
                name = T.concat ["update", T.pack s0, T.pack tn, T.pack s1]
                typE = case mCt of                        
                            OnlySender (ctn,ct)        -> T.concat [name, (.::.),T.pack tn," -> ",T.pack s0," -> (",T.pack s1,", (OnlySender ",T.pack ctn,"))"]
                            AllExceptSender (ctn,ct)   -> T.concat [name, (.::.),T.pack tn," -> ",T.pack s0," -> (",T.pack s1,", (AllExceptSender ",T.pack ctn,"))"]
                            SenderAnd (ctn,ct)         -> T.concat [name, (.::.),T.pack tn," -> ",T.pack s0," -> (",T.pack s1,", (SenderAnd ",T.pack ctn,"))"]
                            ToAll (ctn,ct)             -> T.concat [name, (.::.),T.pack tn," -> ",T.pack s0," -> (",T.pack s1,", (ToAll ",T.pack ctn,"))"]
                            NoClientMessage            -> T.concat [name, (.::.),T.pack tn," -> ",T.pack s0," -> ",T.pack s1]
                decl = T.concat [name, " ", generatePattern (tn,tetd), generatePattern (s0,s0Cons), "=\n    ","error \"Update function ",name," not defined. Please define it in userApp/Update.hs.\""]
            in
                T.unlines
                    [
                        "--Hint: replace the error with the return value of this update function."
                    ,   typE
                    ,   decl
                    ,   ""
                    ]
        createClientUserUpdate :: ((String, ClientTransition), (String, Maybe ServerTransition)) -> T.Text
        createClientUserUpdate ((s0,(tn,tetd)),(s1,mCt)) =
            let
                (.::.) = " : "
                (_,s0Cons) = case M.lookup s0 cStates of
                            Just constr -> constr
                            Nothing -> error $ "State " ++ s0 ++ " does not exist in the map."
                s1Cons = case M.lookup s1 cStates of
                            Just constr -> constr
                            Nothing -> error $ "State " ++ s0 ++ " does not exist in the map."
                name = T.concat ["update", T.pack s0, T.pack tn, T.pack s1]
                typE = case mCt of                        
                            Just (ctn,ct) -> T.concat [name, (.::.),T.pack tn," -> ",T.pack s0," -> (",T.pack s1,", ",T.pack ctn,")"]
                            Nothing       -> T.concat [name, (.::.),T.pack tn," -> ",T.pack s0," -> ",T.pack s1]
                decl = T.concat [name, " ", generatePattern (tn,tetd), generatePattern (s0,s0Cons), "=\n    ","error \"Update function ",name," not defined. Please define it in userApp/Update.hs.\""]
            in
                T.unlines
                    [
                        "--Hint: replace the error with the return value of this update function."
                    ,   typE
                    ,   decl
                    ,   ""
                    ]

                    
        userUpdateHs = [
                        "module Update where"
                       ,"import Static.Types --types are generated from the state diagram, don't remove this import"
                       ,"import Static.ServerTypes"
                       ,""
                       ,"{-"
                       ,"    Fill in the update functions to control the behaviour of your server. Don't change the type declarations or the input definitions"
                       ,"    of the functions. Types have been tagged with M and R for Message and Return types, respectively. These represent constructors"
                       ,"    identical to your messages and states and arguments are unwrapped in the background. This ensures your code follows the state"
                       ,"    diagram you created. If you would like to change these input and return types, edit the state diagram and regenerate the code."
                       ,""
                       ,"    The functions are named as follows: updateCurrentMessageNext where"
                       ,"         - Current is the current server state"
                       ,"         - Message is the message being received"
                       ,"         - Next is the next server state"
                       ,"-}"
                       ,T.unlines $ map createServerUserUpdate $ M.toList sDiagram
                       ]
        userUpdateElm = [
                        "module Update exposing(..)"
                       ,"import Static.Types exposing(..) --types are generated from the state diagram, don't remove this import"
                       ,"import Utils.Utils exposing(..)"
                       ,""
                       ,"{-"
                       ,"    Fill in the update functions to control the behaviour of your server. Don't change the type declarations or the input definitions"
                       ,"    of the functions. Types have been tagged with M and R for Message and Return types, respectively. These represent constructors"
                       ,"    identical to your messages and states and arguments are unwrapped in the background. This ensures your code follows the state"
                       ,"    diagram you created. If you would like to change these input and return types, edit the state diagram and regenerate the code."
                       ,""
                       ,"    The functions are named as follows: updateCurrentMessageNext where"
                       ,"         - Current is the current server state"
                       ,"         - Message is the message being received"
                       ,"         - Next is the next server state"
                       ,"-}"
                       ,T.unlines $ map createClientUserUpdate $ M.toList cDiagram
                       ]
        userTypesHs = ["{-"
                      ,"    Define your own types for internal use here. Note that any types used in the state or messages should be defined in the state"
                      ,"    diagram and generated for use in your program. These types can be used within update / view functions only."
                      ,"-}"
                      ,"module Types where"
                      ]

        userTypesElm = ["{-"
                      ,"    Define your own types for internal use here. Note that any types used in the state or messages should be defined in the state"
                      ,"    diagram and generated for use in your program. These types can be used within update / view functions only."
                      ,"-}"
                      ,"module Types exposing(..)"
                      ]

        userInitHs = ["module Init where"
                     ,"import Static.Types\n"
                     ,"{-"
                     ,"    Define the starting point of your model here. Note: the starting state is defined within your state diagram. Only the starting"
                     ,"    state's data can be modified here. If you'd like a completely different starting state, modify your state diagram and regenerate"
                     ,"    the code. Note that the return type of the function is tagged with \"R\". This is a type identical to your initial state, with"
                     ,"    the same arguments. This \"R\"-type will be unwrapped for you in the background. This ensures consistency with your state diagram."
                     ,"-}\n"
                     ,"-- Hint: replace error with the return value of your initial state"
                     ,T.concat["init :: ",T.pack startSs]
                     ,"init = error \"Initial server state not defined. Please define it in userApp/Init.hs.\""
                     ]

        userInitElm = ["module Init exposing(..)"
                     ,"import Static.Types exposing (..)\n"
                     ,"import Utils.Utils exposing (..)"
                     ,"{-"
                     ,"    Define the starting point of your model here. Note: the starting state is defined within your state diagram. Only the starting"
                     ,"    state's data can be modified here. If you'd like a completely different starting state, modify your state diagram and regenerate"
                     ,"    the code. Note that the return type of the function is tagged with \"R\". This is a type identical to your initial state, with"
                     ,"    the same arguments. This \"R\"-type will be unwrapped for you in the background. This ensures consistency with your state diagram."
                     ,"-}\n"
                     ,"-- Hint: replace error with the return value of your initial state"
                     ,T.concat["init : ",T.pack startCs]
                     ,"init = error \"Initial client state not defined. Please define it in userApp/Init.hs.\""
                     ]

        hiddenUpdateHs = ["module Static.Update where"
                         ,"import Static.Types"
                         ,"import Static.ServerTypes"
                         ,"import Update\n"
                         ,"import Utils.Utils"
                         ,"--Server state unwrappers"
                         ,T.concat $ map (createUnwrap True "Model" "S") $ M.elems sStates
                         ,"--Server state wrappers"
                         ,T.concat $ map (createWrap (length (M.elems sStates) > 1) True "Model" "S") $ M.elems sStates
                         ,"--Server message wrappers"
                         ,T.concat $ map (createWrap (length serverMsgs > 1) True "ServerMessage" "M") serverMsgs
                         ,"--Client message unwrappers"
                         ,T.concat $ map (createUnwrap True "ClientMessage" "M") $ mapMaybe cm2constr serverOutgoingMsgs
                         ,"update :: Int -> ServerMessage -> Model -> IO (Model, Maybe (InternalCM ClientMessage))"
                         ,"update clientId msg model ="
                         ,"    case (msg, model) of"
                         ,T.concat $ map createServerUpdateCase $ M.toList sDiagram
                         ]

        hiddenUpdateElm = ["module Static.Update exposing (..)"
                         ,"import Static.Types exposing (..)"
                         ,"import Update exposing (..)\n"
                         ,"import Utils.Utils exposing(error)"
                         ,"--Server state unwrappers"
                         ,T.concat $ map (createUnwrap False "Model" "S") $ M.elems cStates
                         ,"--Server state wrappers"
                         ,T.concat $ map (createWrap ((length $ M.elems cStates) > 1) False "Model" "S") $ M.elems cStates
                         ,"--Client message wrappers"
                         ,T.concat $ map (createWrap (length clientMsgs > 1) False "ClientMessage" "M") $ clientMsgs
                         ,"--Client message unwrappers"
                         ,T.concat $ map (createUnwrap False "ServerMessage" "M") $ clientOutgoingMsgs
                         ,"update : ClientMessage -> Model -> (Model, Maybe ServerMessage)"
                         ,"update msg model ="
                         ,"    case (msg, model) of"
                         ,T.concat $ map createClientUpdateCase $ M.toList cDiagram
                         ]

        createUnwrap :: Bool -> String -> String -> Constructor -> T.Text
        createUnwrap h outputType inputPrefix (n,args) =
            let
                (.::.) = if h then " :: " else " : "
                name = T.concat ["unwrap", T.pack n]
                typE = T.concat [name, (.::.), T.pack n, " -> ", T.pack outputType]
                decl = T.concat [name," ",generatePattern (n,args)," = ",generatePattern (inputPrefix++n,args)]
            in
                T.unlines
                    [
                        typE
                    ,   decl
                    ,   ""
                    ]

        createWrap :: Bool -> Bool -> String -> String -> Constructor -> T.Text
        createWrap def h inputType outputPrefix (n,args) =
            let
                (.::.) = if h then " :: " else " : "
                name = T.concat ["wrap", T.pack n]
                typE = T.concat [name, (.::.),T.pack inputType," -> ",T.pack n]
                decl = T.concat [name," x__ ="]
            in
                T.unlines
                    [
                        typE
                    ,   decl
                    ,   "    case x__ of"
                    ,   T.concat["        ",generatePattern (outputPrefix++n,args)," -> ",generatePattern (n,args)]
                    ,   if def then T.concat["        _ -> error \"Tried to unwrap a value at the wrong time!\""] else ""
                    ,   ""
                    ]

        createServerUpdateCase :: ((String, ServerTransition), (String, OutgoingClientMessage)) -> T.Text
        createServerUpdateCase ((s0,(tn,tetd)),(s1,mCt)) =
            let
                (.::.) = " :: "
                (_,s0Cons) = case M.lookup s0 sStates of
                            Just constr -> constr
                            Nothing -> error $ "State" ++ s0 ++ " does not exist in the map."
                (_,s1Cons) = case M.lookup s1 sStates of
                            Just constr -> constr
                            Nothing -> error $ "State" ++ s0 ++ " does not exist in the map."
                name = T.concat ["update", T.pack s0, T.pack tn, T.pack s1]
                typE = case mCt of 
                        OnlySender      ct -> T.concat [name, (.::.),"IncomingMessage -> Model -> (",T.pack s1,", OnlySender (",T.pack tn,"))"]
                        AllExceptSender ct -> T.concat [name, (.::.),"IncomingMessage -> Model -> (",T.pack s1,", AllExceptSender (",T.pack tn,"))"]
                        SenderAnd       ct -> T.concat [name, (.::.),"IncomingMessage -> Model -> (",T.pack s1,", SenderAnd (",T.pack tn,"))"]
                        ToAll           ct -> T.concat [name, (.::.),"IncomingMessage -> Model -> (",T.pack s1,", ToEveryone (",T.pack tn,"))"]
                        NoClientMessage    -> T.concat [name, (.::.),"OutgoingMessage -> Model -> ",T.pack s1]
                decl = T.concat [name, " ", generatePattern (tn,tetd), generatePattern (s0,s0Cons), " = error \"Update function ",name," not defined. Please define it in userApp/Update.hs.\""]
                (ctn,ct) = case (cm2constr mCt) of
                            Just ct -> ct
                            Nothing -> ("",[])
                wrapMsg = T.concat ["(wrap",T.pack tn," msg)"]
                wrapModel = T.concat ["(wrap",T.pack s0," model)"]
            in
                case mCt of 
                    OnlySender      ct -> T.concat ["        ","(",generatePattern ("M"++tn,tetd),",",generatePattern ("S"++s0,s0Cons),") -> let (wrappedModel, wrappedMsg) = update",T.pack s0,T.pack tn,T.pack s1," ", wrapMsg," ",wrapModel," in return (unwrap",T.pack s1," wrappedModel, Just <| unwrapOnlySender wrappedMsg unwrap",T.pack ctn,")\n"]
                    AllExceptSender ct -> T.concat ["        ","(",generatePattern ("M"++tn,tetd),",",generatePattern ("S"++s0,s0Cons),") -> let (wrappedModel, wrappedMsg) = update",T.pack s0,T.pack tn,T.pack s1," ", wrapMsg," ",wrapModel," in return (unwrap",T.pack s1," wrappedModel, Just <| unwrapAllExceptSender wrappedMsg unwrap",T.pack ctn,")\n"]
                    SenderAnd       ct -> T.concat ["        ","(",generatePattern ("M"++tn,tetd),",",generatePattern ("S"++s0,s0Cons),") -> let (wrappedModel, wrappedMsg) = update",T.pack s0,T.pack tn,T.pack s1," ", wrapMsg," ",wrapModel," in return (unwrap",T.pack s1," wrappedModel, Just <| unwrapSenderAnd wrappedMsg unwrap",T.pack ctn,")\n"]
                    ToAll           ct -> T.concat ["        ","(",generatePattern ("M"++tn,tetd),",",generatePattern ("S"++s0,s0Cons),") -> let (wrappedModel, wrappedMsg) = update",T.pack s0,T.pack tn,T.pack s1," ", wrapMsg," ",wrapModel," in return (unwrap",T.pack s1," wrappedModel, Just <| unwrapToAll wrappedMsg unwrap",T.pack ctn,")\n"]
                    NoClientMessage    -> T.concat ["        ","(",generatePattern ("M"++tn,tetd),",",generatePattern ("S"++s0,s0Cons),") -> return $ (unwrap",T.pack s1," <| update",T.pack s0,T.pack tn,T.pack s1, " ", wrapMsg," ",wrapModel,", Nothing)\n"]
                    
        createClientUpdateCase :: ((String, ClientTransition), (String, Maybe ServerTransition)) -> T.Text
        createClientUpdateCase ((s0,(tn,tetd)),(s1,mCt)) =
            let
                (.::.) = " : "
                (_,s0Cons) = case M.lookup s0 cStates of
                            Just constr -> constr
                            Nothing -> error $ "State" ++ s0 ++ " does not exist in the map."
                (_,s1Cons) = case M.lookup s1 cStates of
                            Just constr -> constr
                            Nothing -> error $ "State" ++ s0 ++ " does not exist in the map."
                name = T.concat ["update", T.pack s0, T.pack tn, T.pack s1]
                typE = case mCt of 
                        Just ct -> T.concat [name, (.::.),"IncomingMessage -> Model -> (R",T.pack s1,", M",T.pack tn,")"]
                        Nothing -> T.concat [name, (.::.),"OutgoingMessage -> Model -> R",T.pack s1]
                decl = T.concat [name, " ", generatePattern (tn,tetd), generatePattern (s0,s0Cons), " = error \"Update function ",name," not defined. Please define it in userApp/Update.hs.\""]
                (ctn,ct) = case mCt of
                            Just (ctn,ct) -> (ctn,ct)
                            Nothing       -> ("",[])
                wrapMsg = T.concat ["(wrap",T.pack tn," msg)"]
                wrapModel = T.concat ["(wrap",T.pack s0," model)"]
            in
                case mCt of 
                    Just ct -> T.concat ["        ","(",generatePattern ("M"++tn,tetd),",",generatePattern ("S"++s0,s0Cons),") -> let (wrappedModel, wrappedMsg) = update",T.pack s0,T.pack tn,T.pack s1, " ", wrapMsg," ",wrapModel," in (unwrap",T.pack s1," wrappedModel,Just <| unwrap",T.pack ctn," wrappedMsg)\n"]
                    Nothing -> T.concat ["        ","(",generatePattern ("M"++tn,tetd),",",generatePattern ("S"++s0,s0Cons),") -> (unwrap",T.pack s1," <| update",T.pack s0,T.pack tn,T.pack s1," ", wrapMsg," ",wrapModel," , Nothing)\n"]
        

        staticInitHs = [
                            "module Static.Init where\n"
                       ,    "import Static.Types"
                       ,    "import Static.Update"
                       ,    "import qualified Init"
                       ,    "init :: Model",""
                       ,    T.concat["init = unwrap", T.pack startSs, " ", "Init.init"]
                       ]          
        staticInitElm = [
                            "module Static.Init exposing(..)\n"
                       ,    "import Static.Types exposing (..)"
                       ,    "import Static.Update exposing (..)"
                       ,    "import Init",""
                       ,    "init : Model"
                       ,    T.concat["init = unwrap", T.pack startCs, " ", "Init.init"]
                       ]    

    in do
        createDirectoryIfMissing True $ fp </> "client" </> "app"
        createDirectoryIfMissing True $ fp </> "client" </> "src" </> "utils"
        createDirectoryIfMissing True $ fp </> "client" </> "src" </> "static"
        createDirectoryIfMissing True $ fp </> "client" </> "src" </> "userApp"
        createDirectoryIfMissing True $ fp </> "server" </> "app"
        createDirectoryIfMissing True $ fp </> "server" </> "src" </> "utils"
        createDirectoryIfMissing True $ fp </> "server" </> "src" </> "static"
        createDirectoryIfMissing True $ fp </> "server" </> "src" </> "userApp"
        currentTime <- getCurrentTime
        TIO.writeFile (fp </> "server" </> "src" </> "static" </> "Types" <.> "hs") $ T.unlines $ disclaimer currentTime : typesHs
        TIO.writeFile (fp </> "server" </> "src" </> "static" </> "ServerTypes" <.> "hs") $ T.unlines $ disclaimer currentTime : [serverTypesHs]
        TIO.writeFile (fp </> "server" </> "app" </> "Main" <.> "hs") $ T.unlines $ disclaimer currentTime : [mainHs]
        TIO.writeFile (fp </> "server" </> "src" </> "utils" </> "Utils" <.> "hs") $ T.unlines $ disclaimer currentTime : [utilsHs]
        TIO.writeFile (fp </> "server" </> "src" </> "static" </> "Encode" <.> "hs")      $ T.unlines $ disclaimer currentTime : encoderHs
        TIO.writeFile (fp </> "server" </> "src" </> "static" </> "Decode" <.> "hs")      $ T.unlines $ disclaimer currentTime : decoderHs
        TIO.writeFile (fp </> "server" </> "src" </> "static" </> "Update" <.> "hs")      $ T.unlines $ disclaimer currentTime : hiddenUpdateHs
        TIO.writeFile (fp </> "server" </> "src" </> "static" </> "Lib" <.> "hs")      $ T.unlines $ disclaimer currentTime : [libHs]
        TIO.writeFile (fp </> "server" </> "src" </> "static" </> "ServerLogic" <.> "hs")      $ T.unlines $ disclaimer currentTime : [serverLogicHs]
        TIO.writeFile (fp </> "server" </> "src" </> "static" </> "Init" <.> "hs")      $ T.unlines $ disclaimer currentTime : staticInitHs
        _ <- if onlyStatic then return () else (do
            TIO.writeFile (fp </> "server" </> "src" </> "userApp" </> "Update" <.> "hs")      $ T.unlines $ userUpdateHs
            TIO.writeFile (fp </> "server" </> "src" </> "userApp" </> "Types" <.> "hs")      $ T.unlines $ userTypesHs
            TIO.writeFile (fp </> "server" </> "src" </> "userApp" </> "Init" <.> "hs")      $ T.unlines $ userInitHs)

        TIO.writeFile (fp </> "client" </> "elm.json")                     $ jsonElm
        TIO.writeFile (fp </> "client" </> "src" </> "static" </> "Types" <.> "elm") $ T.unlines $ disclaimer currentTime : typesElm
        TIO.writeFile (fp </> "client" </> "src" </> "static" </> "ServerTypes" <.> "elm") $ T.unlines $ disclaimer currentTime : [clientTypesElm]
        TIO.writeFile (fp </> "client" </> "app" </> "Main" <.> "elm") $ T.unlines $ disclaimer currentTime : [mainElm]
        TIO.writeFile (fp </> "client" </> "src" </> "utils" </> "Utils" <.> "elm") $ T.unlines $ disclaimer currentTime : [utilsElm]
        TIO.writeFile (fp </> "client" </> "src" </> "static" </> "Encode" <.> "elm")      $ T.unlines $ disclaimer currentTime : encoderElm
        TIO.writeFile (fp </> "client" </> "src" </> "static" </> "Decode" <.> "elm")      $ T.unlines $ disclaimer currentTime : decoderElm
        TIO.writeFile (fp </> "client" </> "src" </> "static" </> "Update" <.> "elm")      $ T.unlines $ disclaimer currentTime : hiddenUpdateElm
        TIO.writeFile (fp </> "client" </> "src" </> "static" </> "Lib" <.> "elm")      $ T.unlines $ disclaimer currentTime : [libElm]
        TIO.writeFile (fp </> "client" </> "src" </> "static" </> "ClientLogic" <.> "elm")      $ T.unlines $ disclaimer currentTime : [clientLogicElm]
        TIO.writeFile (fp </> "client" </> "src" </> "static" </> "Init" <.> "elm")      $ T.unlines $ disclaimer currentTime : staticInitElm
        TIO.writeFile (fp </> "client" </> "src" </> "static" </> "Version" <.> "elm")      $ T.unlines $ disclaimer currentTime : [versionElm]
        _ <- if onlyStatic then return () else (do
            TIO.writeFile (fp </> "client" </> "src" </> "userApp" </> "Update" <.> "elm")      $ T.unlines $ userUpdateElm
            TIO.writeFile (fp </> "client" </> "src" </> "userApp" </> "Types" <.> "elm")      $ T.unlines $ userTypesElm
            TIO.writeFile (fp </> "client" </> "src" </> "userApp" </> "Init" <.> "elm")      $ T.unlines $ userInitElm
            TIO.writeFile (fp </> "client" </> "src" </> "userApp" </> "View.elm")                     $ viewElm)

        putStrLn $ show serverTransitions