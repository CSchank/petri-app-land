{-# LANGUAGE OverloadedStrings #-}

module Generate.Server where

import                  Control.Monad (unless)
import                  Generate.Codec
import                  Generate.Types
import                  Generate.Standalone
import qualified        Data.Map                as M
import qualified        Data.Set                as S
import qualified        Data.Text               as T
import qualified        Data.Text.IO            as TIO
import                  Types
import                  Utils
import                  Generate.Types
import                  Generate.OneOf
import                  Generate.AllOf
import                  System.Directory
import                  System.FilePath.Posix   ((</>),(<.>))
import                  Data.Maybe              (mapMaybe,fromMaybe)
import                  Data.Time               (getCurrentTime)

cm2maybe :: OutgoingClientMessage -> Maybe OutgoingClientMessage
cm2maybe NoClientMessage       = Nothing
cm2maybe a = Just a

cm2constr :: OutgoingClientMessage -> Maybe [ClientTransition]
cm2constr NoClientMessage           = Nothing
cm2constr (ToSender ct)             = Just [ct]
cm2constr (ToAllExceptSender ct)    = Just [ct]
cm2constr (ToSenderAnd ct)          = Just [ct]
cm2constr (ToSet ct)          = Just [ct]
cm2constr (ToAll ct)                = Just [ct]
cm2constr (OneOf ocms)              = 
    case mapMaybe cm2constr ocms of
        [] -> Nothing
        a -> Just $ concat a
cm2constr (AllOf ocms)              =
    case mapMaybe cm2constr ocms of
        [] -> Nothing
        a -> Just $ concat a

generateStateMsgs :: ClientStateDiagram -> M.Map String [(Constructor, String, Maybe ClientCmd,Maybe ServerTransition)]
generateStateMsgs csD =
    let
        csL = M.toList csD
        createDict currDict ((s0,trans),(s1,mCmd,mSt)) = 
            M.alter (alterOne (trans,s1,mCmd,mSt)) s0 currDict
        alterOne (trans,s1,mCmd,mSt) mState = 
            case mState of 
                Just transs -> Just $ (trans,s1,mCmd,mSt):transs
                Nothing     -> Just [(trans,s1,mCmd,mSt)]
    in
        foldl createDict M.empty csL

generateServer :: Bool -> Bool -> FilePath -> ClientServerApp -> IO ()
generateServer gsvg onlyStatic fp (startCs
                  ,startSs
                  ,cStateslst
                  ,sStateslst
                  ,cExtraTlst
                  ,sExtraTlst
                  ,cDiagram
                  ,sDiagram
                  ) = 
    let
        cState2ConstrMap (ClientState (n,edt)) = (n,(n,edt))
        cState2ConstrMap (ClientStateWithSubs (n,edt) _) = (n,(n,edt))
        cStates = M.fromList $ map cState2ConstrMap cStateslst
        sStates = M.fromList $ map (\(n,const) -> (n,(n,const))) sStateslst
        cExtraT = M.fromList $ map (\(ElmCustom n constrs) -> (n,ElmCustom n constrs)) cExtraTlst
        sExtraT = M.fromList $ map (\(ElmCustom n constrs) -> (n,ElmCustom n constrs)) sExtraTlst
    
        disclaimer date = T.unlines ["{-"
                                ,T.concat["    THIS FILE WAS AUTOMATICALLY GENERATED AT ", T.pack $ show date,"."]
                                , "    IMPORTANT: USE THIS FILE FOR REFERENCE ONLY. YOU SHOULD NOT MODIFY THIS FILE. INSTEAD, MODIFY THE STATE DIAGRAM AND REGENERATE THIS FILE."
                                , "    MODIFYING ANY FILES INSIDE THE static DIRECTORY COULD LEAD TO UNEXPECTED ERRORS IN YOUR APP."
                                ,"-}"
                                ]
        serverStateTypeTxt = 
            generateType True True [DOrd,DEq,DShow] $ ElmCustom "Model" $ map (\(n,t) -> ("S"++n,t)) $ M.elems sStates
        serverMsgs = S.toList $ serverTransFromClient `S.union` serverTransitions
        serverMsgType = ElmCustom "ServerMessage" $ map (\(n,t) -> ("M"++n,t)) serverMsgs
        serverMsgTypeTxt = generateType True True [DOrd,DEq,DShow] serverMsgType

        serverTransFromClient = S.fromList $ mapMaybe (\(_,(_,_,trans)) -> trans) $ M.toList cDiagram
        serverTransitions = S.fromList $ map (\((_,trans),_) -> trans) $ M.toList sDiagram

        serverOutgoingMsgs = S.toList $ S.fromList $ mapMaybe (\(_,(_,trans)) -> cm2maybe trans) $ M.toList sDiagram
        serverOutgoingMsgType = ElmCustom "ClientMessage" $ map (\(n,t) -> ("M"++n,t)) $ concat $ mapMaybe cm2constr serverOutgoingMsgs
        serverOutgoingMsgTypeTxt = generateType True True [DOrd,DEq,DShow] serverOutgoingMsgType


        findOneOfs ct = 
            case ct of
                AllOf cts -> Just $ concat $ mapMaybe findOneOfs cts
                OneOf cts -> Just [length cts]
                _ -> Nothing

        serverOneOfs = S.toList $ S.fromList $ concat $ mapMaybe
            (\(_,(_,mCt)) -> findOneOfs mCt) $ M.toList sDiagram
        
        findAllOfs ct = 
            case ct of
                AllOf cts -> Just [length cts]
                OneOf cts -> Just $ concat $ mapMaybe findAllOfs cts
                _ -> Nothing
        
        serverAllOfs = S.toList $ S.fromList $ concat $ mapMaybe
            (\(_,(_,mCt)) -> findAllOfs mCt) $ M.toList sDiagram
        
        clientStateTypeTxt = 
            generateType False True [DOrd,DEq,DShow] $ ElmCustom "Model" $ map (\(n,_) -> (n,[(ElmType ("View."++n++".Model"),"","")])) $ M.elems cStates

        clientUnionMsgTypeTxt = 
            generateType False True [DOrd,DEq,DShow] $ ElmCustom "ClientMessage" $ map (\(n,_) -> (n,[(ElmType ("View."++n++".Msg"),"","")])) $ M.elems cStates

        clientMsgs = S.toList $ S.fromList $ map (\((_,trans),(_,_,_)) -> trans) $ M.toList cDiagram
        clientMsgType = ElmCustom "WrappedClientMessage" $ map (\(n,t) -> ("M"++n,t)) clientMsgs
        clientMsgTypeTxt = generateType False True [DOrd,DEq,DShow] clientMsgType

        clientOutgoingMsgs = S.toList $ S.fromList $ mapMaybe (\(_,(_,_,trans)) -> trans) $ M.toList cDiagram
        clientOutgoingMsgType = ElmCustom "ServerMessage" $ map (\(n,t) -> ("M"++n,t)) clientOutgoingMsgs
        clientOutgoingMsgTypeTxt = generateType False True [DOrd,DEq,DShow] clientOutgoingMsgType

        serverWrappedMessagesTxt = T.unlines $ map (\(name,constrs) -> generateType True True [DOrd,DEq,DShow] $ ElmCustom name [(name,constrs)]) serverMsgs
        serverWrappedStateTypesTxt = T.unlines $ map (\(name,constrs) -> generateType True True [DOrd,DEq,DShow] $ ElmCustom name [(name,constrs)]) $ M.elems sStates
        serverWrappedOutgoingMsgTxt = T.unlines $ map (\(name,constrs) -> generateType True True [DOrd,DEq,DShow] $ ElmCustom name [(name,constrs)]) $ concat $ mapMaybe cm2constr serverOutgoingMsgs

        serverExtraTypes = map snd $ M.toList sExtraT
        serverExtraTypesTxt = T.unlines $ map (generateType True True [DOrd,DEq,DShow]) serverExtraTypes
        
        clientExtraTypes = map snd $ M.toList cExtraT
        clientExtraTypesTxt = T.unlines $ map (generateType False True [DOrd,DEq,DShow]) clientExtraTypes

        clientWrappedMessagesTxt = T.unlines $ map (\(name,constrs) -> generateType False True [DOrd,DEq,DShow] $ ElmCustom name [(name,constrs)]) clientMsgs
        clientWrappedStateTypesTxt = T.unlines $ map (\(name,constrs) -> generateType False True [DOrd,DEq,DShow] $ ElmCustom name [(name,constrs)]) $ M.elems cStates
        clientWrappedOutgoingMsgTxt = T.unlines $ map (\(name,constrs) -> generateType False True [DOrd,DEq,DShow] $ ElmCustom name [(name,constrs)]) clientOutgoingMsgs

        clientS2M = M.toList $ generateStateMsgs cDiagram

        clientViewModules =
            map (\(n,m) -> (n,clientViewModule (n,m))) clientS2M
        
        clientUpdateModules =
            map (\(n,m) -> (n,clientUpdateModule (n,m))) clientS2M        
        
        clientWrapModules =
            map (\(n,m) -> (n,hiddenWrapModule (n,m))) clientS2M

        clientS2Subs = mapMaybe (\cs -> 
                        case cs of 
                            ClientStateWithSubs (stn,_) subs -> Just (stn,subs)
                            _ -> Nothing 
                        ) cStateslst

        clientSubModules =
            map (\(n,m) -> (n,clientSubsModule (n,m))) clientS2Subs

        clientViewModule (stateName, messages) =
            let 
                stateTxt = T.pack stateName
                justMessages = map (\(m,_,_,_) -> m) messages
                updateNames = map (\(n,_) -> T.concat["update",T.pack n]) justMessages
                moduleType = ElmCustom "Msg" justMessages
                state = fromMaybe ("", []) (M.lookup stateName cStates)
                stateType = ElmCustom "Model" [state]
            in
                T.unlines 
                    [
                        T.concat ["module View.",stateTxt, " exposing(view,title,Model(..),Msg(..))\n"]
                    ,   if gsvg then 
                            "import GraphicSVG exposing(..)"
                        else T.unlines 
                        [   "import Html exposing(..)"
                        ,   "import Html.Attributes exposing(..)"
                        ,   "import Html.Events exposing(..)\n"
                        ]
                    ,   "import Static.Types exposing(..)"
                    ,   "import Utils.Utils exposing(error)"
                    ,   "import Dict exposing (Dict)"
                    ,   "-- These types are provided for reference only. Changing them will result in a failure to compile."
                    ,   "-- The model of the state"
                    ,   generateType False True [DOrd,DEq,DShow] stateType,""
                    ,   "-- The possible messages to send while in this state"
                    ,   generateType False True [DOrd,DEq,DShow] moduleType,"\n"
                    ,   "--Change the title of the tab / browser here."
                    ,   "title : Model -> String"
                    ,   "title model = "
                    ,   T.concat["    \"",T.pack stateName,"\""],""
                    ,   "--Define your view function for this state here."
                    ,   if gsvg then "view : Model -> Collage Msg" else "view : Model -> Html Msg"
                    ,   "view model = "
                    ,   T.concat["    error \"Please define the view function for the state ", T.pack stateName,"\""]
                    ]

        --clientUpdateModule :: (String, (Constructor, String, Maybe ServerTransition))
        clientUpdateModule (stateName, messages) =
            let 
                stateTxt = T.pack stateName
                justMessages = map (\(m,_,_,_) -> m) messages
                msgNames = map (\((n,_),_,_,_) -> T.pack n) messages
                updateNames = map (\((n,_),s1,_,_) -> T.concat["update",T.pack stateName,T.pack n,T.pack s1]) messages
                moduleType = ElmCustom "Msg" justMessages
                state = fromMaybe ("", []) (M.lookup stateName cStates)
                stateType = ElmCustom "Model" [state]
            in
                T.unlines 
                    [
                        T.concat ["module Update.",stateTxt," exposing\n    ( ",T.intercalate "\n    , " updateNames,"\n    )\n"]
                    ,   T.concat ["import Static.Types exposing(..)"],"\n"
                    ,   "import Utils.Utils exposing(error)"
                    ,   T.unlines $ map (\(m,s1,mCmd,mSt) -> createClientUserUpdate ((stateName,m),(s1,mCmd,mSt))) messages,"\n"
                    ]

        clientSubsModule (stateName, subs) =
            let 
                stateTxt = T.pack stateName
                justSubs = map (\(m,_) -> m) subs
                msgNames = map (\(n,_) -> T.pack n) subs
                subsNames = map (\(n,_) -> T.concat["sub",T.pack stateName,T.pack n]) subs
                moduleType = ElmCustom "Msg" subs
                state = fromMaybe ("", []) (M.lookup stateName cStates)
            in
                T.unlines 
                    [
                        T.concat ["module Subs.",stateTxt," exposing (..)"]
                    ,   T.concat ["import Static.Types exposing(..)"]
                    ,   T.concat ["import View.",stateTxt," exposing (Model)"],"\n"
                    ,   "import Utils.Utils exposing(error)"
                    ,   T.unlines $ map createClientUserSub subs,"\n"
                    ]
        
        createClientUserSub :: Constructor -> T.Text
        createClientUserSub (sn,_) =
            let
                (.::.) = " : "
                name = T.concat ["sub", T.pack sn]
                decl = T.concat [name, " m =\n    ","error \"Subscription function ",name," not defined. Please define it in userApp/Subs/",T.pack sn,".elm\""]
                typE = T.concat [name, (.::.), "Model -> Sub ", T.pack sn]
            in
                T.unlines
                    [
                        "--Hint: replace the error with the subscription."
                    ,   typE
                    ,   decl
                    ,   ""
                    ]

        hiddenWrapModule (stateName,messages) = 
            let 
                stateTxt = T.pack stateName
                justMessages = map (\(m,_,_,_) -> m) messages
                msgNames = map (\((n,_),_,_,_) -> T.pack n) messages
                updateNames = map (\(n,_) -> T.concat["update",T.pack n]) justMessages
                moduleType = ElmCustom "Msg" justMessages
                state = fromMaybe ("", []) (M.lookup stateName cStates)
                stateType = ElmCustom "Model" [state]
            in
                T.unlines
                    [T.concat["module Static.Wrappers.",T.pack stateName," exposing (..)"]
                    ,"import Static.Types exposing (..)"
                    ,"import Utils.Utils exposing(error)"
                    ,T.concat["import View.",T.pack stateName," exposing(Model,Msg)"] 
                    ,"--Client message wrappers"
                    ,T.concat $ map (createWrap (length justMessages > 1) False "Msg" ("View."++stateName++".")) justMessages
                    ,"--Client message unwrappers"                    
                    ,T.concat $ map (createUnwrap False "Msg" ("View."++stateName++".")) justMessages
                    ,"unwrapCMessage: WrappedClientMessage -> Msg"
                    ,"unwrapCMessage cMsg = case cMsg of"
                    ,T.unlines $ map (\(n,et) -> T.concat ["    ",generatePattern ("M"++n,et)," -> ",generatePattern ("View."++stateName++"."++n,et)]) justMessages
                    ,if length justMessages < length clientMsgs then T.concat["    _ -> error \"Incorrect message passed to ",T.pack stateName," state.\""] else ""
                    ,"--Client message wrappers"
                    ,"wrapCMessage: Msg -> WrappedClientMessage"
                    ,"wrapCMessage msg = case msg of"
                    ,T.unlines $ map (\(n,et) -> T.concat ["    ",generatePattern ("View."++stateName++"."++n,et)," -> ",generatePattern ("M"++n,et)]) justMessages
                    ]

        hiddenClientView =
            T.unlines
                    [T.concat["module Static.View exposing (view,title)"]
                    ,"import Static.Types exposing (..)"
                    ,"import Static.Msg exposing (ClientMessage)"
                    ,"import Utils.Utils exposing(error)"
                    ,"import Static.Model exposing (Model)"
                    ,if gsvg then "import GraphicSVG exposing(..)" else "import Html exposing(..)"
                    ,T.unlines $ map (\(n,_) -> T.concat["import View.",T.pack n]) clientS2M
                    ,T.unlines $ map (\(n,_) -> T.concat["import Static.Wrappers.",T.pack n," exposing(wrapCMessage)"]) clientS2M
                    --,"-- Client states"
                    --,generateType False False $ ElmCustom "Model" $ map (\(n,t) -> (n,[(ElmType ("View."++n++".Model"),"","")])) $ M.elems cStates,""
                    ,"title : Model -> String"
                    ,"title model ="
                    ,"    case model of"
                    ,T.unlines $ map (\(n,_) -> T.concat["        Static.Model.",T.pack n," m -> View.",T.pack n,".title m"]) clientS2M
                    ,if gsvg then "view : Model -> Collage Static.Types.WrappedClientMessage" else "view : Model -> Html Static.Types.WrappedClientMessage"
                    ,"view model ="
                    ,"    case model of"
                    ,T.unlines $ map (\(n,_) -> T.concat["        Static.Model.",T.pack n, if gsvg then T.concat[" m -> mapCollage ","Static.Wrappers.",T.pack n,".wrapCMessage <| View."] else " m -> Html.map wrapCMessage <| View.",T.pack n,".view m"]) clientS2M
                    ]
        
        typesHs = 
                [
                 "{-"
                ,"    Looking for a place to define your own algebraic data types for internal use? Look at you becoming a real Haskell programmer! What a great idea."
                ,"    However, instead of defining them here, why not define them in userApp/Types.hs instead? We've conveniently created it for you :)"
                ,"-}\n"
                ,"module Static.Types where\n"
                ,"import qualified Data.Map.Strict as Dict"
                ,"type List a = [a]"
                ,"type ClientID = Int"
                ,"type Dict k v = Dict.Map k v"
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
                ,"import Dict exposing (Dict)"
                ,"-- Client transitions"
                ,clientMsgTypeTxt,""
                ,"-- Outgoing Messages"
                ,clientOutgoingMsgTypeTxt,""
                ,"-- Extra internal types"
                ,clientExtraTypesTxt,""
                ,"-- Wrapped state types"
                ,clientWrappedStateTypesTxt
                ,"-- Wrapped outgoing messages"
                ,clientWrappedOutgoingMsgTxt
                ,"-- Wrapped client messages"
                ,clientWrappedMessagesTxt
                ]        
        
        modelElm = [
                   "module Static.Model exposing(Model(..))"
                   ,T.unlines $ map (\(n,_) -> T.concat["import View.",T.pack n]) clientS2M
                   ,"-- Client states"
                   ,clientStateTypeTxt,""
                   ]

        msgElm = [
                   "module Static.Msg exposing(ClientMessage(..))"
                   ,T.unlines $ map (\(n,_) -> T.concat["import View.",T.pack n]) clientS2M
                   ,"-- Client states"
                   ,clientUnionMsgTypeTxt,""
                   ]

        {-Encoders-}
        serverOutgoingEncoder = generateEncoder True $ ElmCustom "ClientMessage" $ map (\(n,t) -> ("M"++n,t)) $ concat $ mapMaybe cm2constr serverOutgoingMsgs
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
                ocm2Txt (ToSender (ctn,_))          = T.concat["ToSender ",T.pack ctn]
                ocm2Txt (ToAllExceptSender (ctn,_)) = T.concat["ToAllExceptSender ",T.pack ctn]
                ocm2Txt (ToSenderAnd (ctn,_))       = T.concat["ToSenderAnd ",T.pack ctn]
                ocm2Txt (ToSet (ctn,_))             = T.concat["ToSet ",T.pack ctn]
                ocm2Txt (ToAll (ctn,_))             = T.concat["ToAll ",T.pack ctn]
                ocm2Txt (OneOf ocms)                = T.concat["OneOf",T.pack $ show $ length ocms," (",T.intercalate ") (" $ map ocm2Txt ocms,")"]
                ocm2Txt (AllOf ocms)                = T.concat["AllOf",T.pack $ show $ length ocms," (",T.intercalate ") (" $ map ocm2Txt ocms,")"]
                typE = case mCt of                                                    
                            NoClientMessage -> T.concat [name, (.::.),"ClientID -> ",T.pack tn," -> ",T.pack s0," -> ",T.pack s1]
                            ocm             -> T.concat [name, (.::.),"ClientID -> ",T.pack tn," -> ",T.pack s0," -> (",T.pack s1,",",ocm2Txt ocm,")"]

                decl = T.concat [name, " clientId ", generatePattern (tn,tetd), generatePattern (s0,s0Cons), "=\n    ","error \"Update function ",name," not defined. Please define it in userApp/Update.hs.\""]
            in
                T.unlines
                    [
                        "--Hint: replace the error with the return value of this update function."
                    ,   typE
                    ,   decl
                    ,   ""
                    ]
        createClientUserUpdate :: ((String, ClientTransition), (String, Maybe ClientCmd, Maybe ServerTransition)) -> T.Text
        createClientUserUpdate ((s0,(tn,tetd)),(s1,mCmd,mCt)) =
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

                    
        userUpdateHs = 
            let
                oneOfs = 
                    mapMaybe 
                        (\(_,(_,mCt)) -> 
                            case mCt of
                                OneOf cts -> Just $ length cts
                                _ -> Nothing
                        ) $ M.toList sDiagram
                allOfs = 
                    mapMaybe 
                        (\(_,(_,mCt)) -> 
                            case mCt of
                                AllOf cts -> Just $ length cts
                                _ -> Nothing
                        ) $ M.toList sDiagram
            in
                [
                "module Update where"
                ,"import Static.Types --types are generated from the state diagram, don't remove this import"
                ,"import Static.ServerTypes"
                ,T.unlines $ map (\n -> T.concat ["import Static.OneOf.OneOf",T.pack $ show n," as OneOf",T.pack $ show n]) oneOfs
                ,T.unlines $ map (\n -> T.concat ["import Static.AllOf.AllOf",T.pack $ show n," (AllOf",T.pack $ show n,"(..))"]) allOfs
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
                     ,case startCs of
                        (n, Just (cmdn,_)) -> T.concat["init : (",T.pack n,", Cmd ",T.pack cmdn,")"]
                        (_,Nothing)        -> T.concat ["init : ",T.pack $ fst startCs]
                     ,"init = error \"Initial client state not defined. Please define it in userApp/Init.hs.\""
                     ]

        hiddenUpdateHs = ["module Static.Update where"
                         ,"import Static.Types"
                         ,"import Static.ServerTypes"
                         ,T.unlines $ map (\n -> T.concat ["import Static.OneOf.OneOf",T.pack $ show n]) serverOneOfs
                         ,T.unlines $ map (\n -> T.concat ["import Static.AllOf.AllOf",T.pack $ show n]) serverAllOfs
                         ,"import Update\n"
                         ,"import Utils.Utils"
                         ,"--Server state unwrappers"
                         ,T.concat $ map (createUnwrap True "Model" "S") $ M.elems sStates
                         ,"--Server state wrappers"
                         ,T.concat $ map (createWrap (length (M.elems sStates) > 1) True "Model" "S") $ M.elems sStates
                         ,"--Server message wrappers"
                         ,T.concat $ map (createWrap (length serverMsgs > 1) True "ServerMessage" "M") serverMsgs
                         ,"--Client message unwrappers"
                         ,T.concat $ map (createUnwrap True "ClientMessage" "M") $ concat $ mapMaybe cm2constr serverOutgoingMsgs
                         ,"update :: ClientID -> ServerMessage -> Model -> IO (Model, Maybe (InternalCM ClientMessage))"
                         ,"update clientId msg model ="
                         ,"    case (msg, model) of"
                         ,T.concat $ map createServerUpdateCase $ M.toList sDiagram
                         ]

        hiddenUpdateElm = ["module Static.Update exposing (..)"
                         ,"import Static.Types exposing (..)"
                         ,"import Static.Model exposing (Model)"
                         ,"import Static.Msg exposing (ClientMessage)"
                         ,T.unlines $ map (\(n,_) -> T.pack $ "import Update."++n++" exposing(..)") clientS2M
                         ,T.unlines $ map (\(n,_) -> T.pack $ "import View." ++ n) clientS2M
                         ,T.unlines $ map (\(n,_) -> T.pack $ "import Static.Wrappers." ++ n) clientS2M
                         ,"import Utils.Utils exposing(error)"
                         ,"--Client state unwrappers"
                         ,T.concat $ map (\(n,et) -> createUnwrap False ("View."++n++".Model") ("View."++n++".") (n,et)) $ M.elems cStates
                         ,"--Client state wrappers"
                         ,T.concat $ map (\(n,et) -> createWrap False False ("View."++n++".Model") ("View."++n++".") (n,et)) $ M.elems cStates
                         ,"--Outgoing message unwrappers"
                         ,T.concat $ map (\(n,et) -> createUnwrap False "ServerMessage" "M" (n,et)) $ clientOutgoingMsgs
                         ,"update : WrappedClientMessage -> Model -> (Model, Maybe (Cmd WrappedClientMessage), Maybe ServerMessage)"
                         ,"update msg model ="
                         ,"    case (msg,model) of"
                         ,T.concat $ map createClientUpdateCase $ M.toList cDiagram
                         ,if M.size cStates * length clientMsgs > M.size cDiagram then "        _ -> error \"The current state received a message it didn't know how to handle!\"" else ""
                         ]
        hiddenSubsElm = ["module Static.Subs exposing (..)"
                         ,"import Static.Types exposing (..)"
                         ,"import Static.Model exposing (Model)"
                         ,"import Static.Msg"
                         ,T.unlines $ map (\(n,_) -> T.pack $ "import View." ++ n) clientS2Subs
                         ,T.unlines $ map (\(n,_) -> T.pack $ "import Subs." ++ n) clientS2Subs
                         ,T.unlines $ map (\(n,_) -> T.pack $ "import Static.Wrappers." ++ n) clientS2Subs
                         ,"subscriptions : Model -> Sub WrappedClientMessage"
                         ,"subscriptions model ="
                         ,"    case model of"
                         ,T.concat $ map createClientSubCase clientS2Subs
                         ,if length clientS2Subs < M.size cDiagram then "        _ ->  Sub.none" else ""
                         ]

        createClientSubCase :: (String, [Constructor]) -> T.Text
        createClientSubCase (stateName, subs) =
            let
                snTxt = T.pack stateName
                subsTxt = map (\(sn,_) -> T.concat ["Sub.map (Static.Wrappers.",snTxt,".wrapCMessage << Static.Wrappers.",snTxt,".unwrap",T.pack sn,") <| Subs.",T.pack stateName,".sub",T.pack sn," m"]) subs
            in
                T.concat ["        Static.Model.",snTxt," m -> Sub.batch [",T.intercalate "," subsTxt,"]"]

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
                    ,   if def then T.concat["        _ -> error \"Tried to wrap a value at the wrong time!\""] else ""
                    ,   ""
                    ]

        createServerUpdateCase :: ((String, ServerTransition), (String, OutgoingClientMessage)) -> T.Text
        createServerUpdateCase ((s0,(tn,tetd)),(s1,mCt)) =
            let
                (_,s0Cons) = case M.lookup s0 sStates of
                            Just constr -> constr
                            Nothing -> error $ "State" ++ s0 ++ " does not exist in the map."
                (_,s1Cons) = case M.lookup s1 sStates of
                            Just constr -> constr
                            Nothing -> error $ "State" ++ s0 ++ " does not exist in the map."
                cts = case (cm2constr mCt) of
                            Just ct -> ct
                            Nothing -> []
                wrapMsg = T.concat ["(wrap",T.pack tn," msg)"]
                wrapModel = T.concat ["(wrap",T.pack s0," model)"]
                unwrapMsg (ctn,ct) = T.concat["unwrap",T.pack ctn]
                unwrapMct mCt =
                    case mCt of
                        ToSender            ct -> T.concat ["unwrapToSender ",unwrapMsg ct]
                        ToAllExceptSender   ct -> T.concat ["unwrapToAllExceptSender ",unwrapMsg ct]
                        ToSet               ct -> T.concat ["unwrapToSet", unwrapMsg ct]
                        ToSenderAnd         ct -> T.concat ["unwrapToSenderAnd ",unwrapMsg ct]
                        ToAll               ct -> T.concat ["unwrapToAll ",unwrapMsg ct]
                        OneOf              cts -> T.concat ["Static.OneOf.OneOf",T.pack $ show $ length cts,".unwrap (",T.intercalate ") (" $ map unwrapMct cts,")"]
                        AllOf              cts -> T.concat ["ICMAllOf . Static.AllOf.AllOf",T.pack $ show $ length cts,".unwrap (",T.intercalate ") (" $ map unwrapMct cts,")"]
                        NoClientMessage        -> "ICMNoClientMessage"
            in
                case mCt of
                    NoClientMessage -> T.concat ["        ","(",generatePattern ("M"++tn,tetd),",",generatePattern ("S"++s0,s0Cons),") -> return $ (unwrap",T.pack s1," <| update",T.pack s0,T.pack tn,T.pack s1," clientId ", wrapMsg," ",wrapModel,", Nothing)\n"] 
                    mCt0            -> T.concat ["        ","(",generatePattern ("M"++tn,tetd),",",generatePattern ("S"++s0,s0Cons),") -> let (wrappedModel, wrappedMsg) = update",T.pack s0,T.pack tn,T.pack s1," clientId ", wrapMsg," ",wrapModel," in return (unwrap",T.pack s1," wrappedModel, Just <| ",unwrapMct mCt0," wrappedMsg)\n"]                    

        createClientUpdateCase :: ((String, ClientTransition), (String, Maybe ClientCmd, Maybe ServerTransition)) -> T.Text
        createClientUpdateCase ((s0,(tn,tetd)),(s1,mCmd,mCt)) =
            let
                ctn = case mCt of
                    Just (n,ct) -> T.pack n
                    Nothing     -> ""
                cmdn = case mCmd of
                    Just (n,_) -> T.pack n
                    Nothing    -> ""
                wrapMsg = T.concat ["(Static.Wrappers.",T.pack s0,".wrap",T.pack tn," <| Static.Wrappers.",T.pack s0,".unwrapCMessage msg)"] --T.concat ["(Static.Wrappers.",T.pack s0,".wrap",T.pack tn," msg)"]
                wrapModel = T.concat ["(wrap",T.pack s0," m)"]
            in
                case (mCmd,mCt) of 
                    (Just _, Just _)    -> T.concat ["        ","(",generatePattern ("M"++tn,tetd),",Static.Model.",T.pack s0," m) -> let (wrappedModel, wrappedCmd,wrappedMsg) = update",T.pack s0,T.pack tn,T.pack s1, " ", wrapMsg," ",wrapModel," in (Static.Model.",T.pack s1," <| unwrap",T.pack s1," wrappedModel,Just <| Cmd.map (Static.Wrappers.",T.pack s0,".wrapCMessage << unwrap",cmdn,") wrappedCmd,Just <| unwrap",ctn," wrappedMsg)\n"]
                    (Nothing, Just _)   -> T.concat ["        ","(",generatePattern ("M"++tn,tetd),",Static.Model.",T.pack s0," m) -> let (wrappedModel, wrappedMsg) = update",T.pack s0,T.pack tn,T.pack s1, " ", wrapMsg," ",wrapModel," in (Static.Model.",T.pack s1," <| unwrap",T.pack s1," wrappedModel,Nothing,Just <| unwrap",ctn," wrappedMsg)\n"]
                    (Just _,Nothing)    -> T.concat ["        ","(",generatePattern ("M"++tn,tetd),",Static.Model.",T.pack s0," m) -> let (wrappedModel, wrappedCmd) = update",T.pack s0,T.pack tn,T.pack s1, " ", wrapMsg," ",wrapModel," in (Static.Model.",T.pack s1," <| unwrap",T.pack s1," wrappedModel,Just <| Cmd.map unwrap",cmdn," wrappedCmd,Nothing)\n"]
                    (Nothing, Nothing)  -> T.concat ["        ","(",generatePattern ("M"++tn,tetd),",Static.Model.",T.pack s0," m) -> (Static.Model.",T.pack s1," <| unwrap",T.pack s1," <| update",T.pack s0,T.pack tn,T.pack s1," ", wrapMsg," ",wrapModel,", Nothing, Nothing)\n"]
        

        staticInitHs = [
                            "module Static.Init where\n"
                       ,    "import Static.Types"
                       ,    "import Static.Update"
                       ,    "import qualified Init"
                       ,    "init :: Model",""
                       ,    T.concat["init = unwrap", T.pack startSs, " ", "Init.init"]
                       ]          
        staticInitElm = let
                            startN = T.pack $ fst startCs
                        in
                        [
                            "module Static.Init exposing(..)\n"
                       ,    "import Static.Types exposing (..)"
                       ,    "import Static.Update exposing (..)"
                       ,    "import Static.Model exposing (Model)"
                       ,    T.concat["import Static.Wrappers.",startN]
                       ,    "import Init",""
                       ,    "init : (Model, Cmd WrappedClientMessage)"
                       ,    case startCs of
                                (n, Just (cmdn,_)) -> T.concat["init = (Static.Model.",startN," <| unwrap", startN, " <| Tuple.first Init.init, Cmd.map (Static.Wrappers.",startN,".wrapCMessage << Static.Wrappers.",startN,".unwrap",T.pack cmdn,") <| Tuple.second Init.init)"]
                                (n, Nothing)   -> T.concat["init = (Static.Model.",startN," <| unwrap", startN, " ", "Init.init, Cmd.none)"]
                       ]    

    in do
        copyDirectory "ClientTemplate/" (fp </> "client")
        copyDirectory "ServerTemplate/" (fp </> "server")
        currentTime <- getCurrentTime
        unless (null serverOneOfs) $ createDirectoryIfMissing True $ fp </> "server" </> "src" </> "Static" </> "OneOf" 
        unless (null serverAllOfs) $ createDirectoryIfMissing True $ fp </> "server" </> "src" </> "Static" </> "AllOf"
        unless (null clientS2Subs) $ createDirectoryIfMissing True $ fp </> "client" </> "src" </> "userApp" </> "Subs"
        mapM_ (\n -> TIO.writeFile (fp </> "server" </> "src" </> "Static" </> "OneOf" </> "OneOf" ++ show n <.> "hs") $ generateOneOf True n) serverOneOfs
        mapM_ (\n -> TIO.writeFile (fp </> "server" </> "src" </> "Static" </> "AllOf" </> "AllOf" ++ show n <.> "hs") $ generateAllOf True n) serverAllOfs
        TIO.writeFile (fp </> "server" </> "src" </> "Static" </> "Types" <.> "hs")  $ T.unlines $ disclaimer currentTime : typesHs
        TIO.writeFile (fp </> "server" </> "src" </> "Static" </> "Encode" <.> "hs") $ T.unlines $ disclaimer currentTime : encoderHs
        TIO.writeFile (fp </> "server" </> "src" </> "Static" </> "Decode" <.> "hs") $ T.unlines $ disclaimer currentTime : decoderHs
        TIO.writeFile (fp </> "server" </> "src" </> "Static" </> "Update" <.> "hs") $ T.unlines $ disclaimer currentTime : hiddenUpdateHs
        TIO.writeFile (fp </> "server" </> "src" </> "Static" </> "Init" <.> "hs")   $ T.unlines $ disclaimer currentTime : staticInitHs
        unless onlyStatic (do
            writeIfNotExists (fp </> "server" </> "src" </> "userApp" </> "Update" <.> "hs") $ T.unlines userUpdateHs
            writeIfNotExists (fp </> "server" </> "src" </> "userApp" </> "Types" <.> "hs")  $ T.unlines userTypesHs
            writeIfNotExists (fp </> "server" </> "src" </> "userApp" </> "Init" <.> "hs")   $ T.unlines userInitHs)


        TIO.writeFile (fp </> "client" </> "src" </> "static" </> "Types" <.> "elm") $ T.unlines $ disclaimer currentTime : typesElm
        --TIO.writeFile (fp </> "client" </> "app" </> "Main" <.> "elm") $ T.unlines $ disclaimer currentTime : [if gsvg then mainElmGSVG else mainElm]
        TIO.writeFile (fp </> "client" </> "src" </> "static" </> "Encode" <.> "elm")      $ T.unlines $ disclaimer currentTime : encoderElm
        TIO.writeFile (fp </> "client" </> "src" </> "static" </> "Decode" <.> "elm")      $ T.unlines $ disclaimer currentTime : decoderElm
        TIO.writeFile (fp </> "client" </> "src" </> "static" </> "Update" <.> "elm")      $ T.unlines $ disclaimer currentTime : hiddenUpdateElm
        TIO.writeFile (fp </> "client" </> "src" </> "static" </> "Model" <.> "elm")      $ T.unlines $ disclaimer currentTime : modelElm
        TIO.writeFile (fp </> "client" </> "src" </> "static" </> "Msg" <.> "elm")      $ T.unlines $ disclaimer currentTime : msgElm
        TIO.writeFile (fp </> "client" </> "src" </> "static" </> "Init" <.> "elm")      $ T.unlines $ disclaimer currentTime : staticInitElm
        TIO.writeFile (fp </> "client" </> "src" </> "static" </> "View" <.> "elm")      $ T.unlines $ disclaimer currentTime : [hiddenClientView]
        TIO.writeFile (fp </> "client" </> "src" </> "static" </> "Subs" <.> "elm")      $ T.unlines $ disclaimer currentTime : hiddenSubsElm
        
        mapM_ (\(n,txt) -> TIO.writeFile (fp </> "client" </> "src" </> "static" </> "Wrappers" </> n <.> "elm") txt) clientWrapModules
        unless onlyStatic (do
            mapM_ (\(n,txt) -> writeIfNotExists (fp </> "client" </> "src" </> "userApp" </> "View" </> n <.> "elm") txt) clientViewModules
            mapM_ (\(n,txt) -> writeIfNotExists (fp </> "client" </> "src" </> "userApp" </> "Update" </> n <.> "elm") txt) clientUpdateModules
            mapM_ (\(n,txt) -> writeIfNotExists (fp </> "client" </> "src" </> "userApp" </> "Subs" </> n <.> "elm") txt) clientSubModules
            --writeIfNotExists (fp </> "client" </> "src" </> "userApp" </> "Update" <.> "elm")      $ T.unlines $ userUpdateElm
            writeIfNotExists (fp </> "client" </> "src" </> "userApp" </> "Types" <.> "elm")      $ T.unlines userTypesElm
            writeIfNotExists (fp </> "client" </> "src" </> "userApp" </> "Init" <.> "elm")      $ T.unlines userInitElm)

        createDirectoryIfMissing True $ fp </> "client" </> "src" </> "Static" </> "Standalone" 
        generateStandalones cExtraTlst fp cStateslst

        print serverTransitions

writeIfNotExists :: FilePath -> T.Text -> IO ()
writeIfNotExists fp txt = do
    exists <- doesFileExist fp
    Prelude.putStrLn $ fp ++ " exists:" ++ show exists
    unless exists $ TIO.writeFile fp txt