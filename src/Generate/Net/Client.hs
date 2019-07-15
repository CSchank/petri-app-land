{-# LANGUAGE OverloadedStrings #-}


module Generate.Net.Client where

import Types
import qualified Data.Map as M
import qualified Data.Text as T
import Generate.Types
import TypeHelpers
import Utils
import                  System.FilePath.Posix   ((</>),(<.>))
import System.Directory
import Data.Maybe (mapMaybe, isJust)
import Generate.Helpers
import Generate.Codec
import Generate.Wrappers
import Generate.Plugins
import Generate.Standalone

trans2constr :: Transition -> Constructor
trans2constr trans = 
    case trans of
        Transition tt constr _ _ -> constr
        ClientTransition constr _ -> constr
        CmdTransition constr _ _    -> constr


place2edts :: Place -> [DocTypeT]
place2edts (Place _ _ _ edts _) = edts
    

transName from msgN = T.concat [ msgN,"from",from]

netTrans2MConstr :: Transition -> Maybe Constructor
netTrans2MConstr trans = 
    case trans of
        Transition tt constr _ _ -> if tt == OriginClientOnly || tt == OriginEitherPossible then Just constr else Nothing
        CmdTransition constr _ _-> Just constr
        ClientTransition {} -> Nothing


generate :: M.Map T.Text CustomT -> FilePath -> Net -> IO ()
generate extraTypes fp net =
    case net of 
        (Net name startingPlace places transitionsFromUser plugins) ->
            let
                {--dummyTransitions =
                  mapMaybe
                    (\placeName ->
                      if length (transFromPlace placeName transitionsFromUser) == 0 then
                        Just (ClientTransition (msg (T.concat["Dummy",placeName]) []) placeName)
                      else Nothing
                    ) placeNames-}
                transitions = transitionsFromUser -- ++ dummyTransitions
                inits = T.unlines
                    [
                    T.concat ["module ", name, ".Init exposing(..)"]
                    ,T.concat["import ",name,".Static.Types exposing(..)"]
                    , ""
                    , "-- the initial state of the starting place"
                    , T.concat ["init : ",startingPlace]
                    , T.concat ["init = ",constr2Def extraTypes (getClientState $ getPlace startingPlace)]
                    ]
                placeNames = map (\(Place name _ _ _ _) -> name) places
                types =
                    let
                        transConstrs = concatMap (snd . trans2constr) $ outgoingClientTransitions False
                        placeConstrs = concatMap place2edts places
                        imports = fnub $ concatMap (findImports Elm) $ transConstrs ++ placeConstrs 
                    in
                    T.unlines $
                    [
                        T.concat ["module ", name, ".Static.Types exposing(..)"]
                    ,   T.concat ["import ", name,".Static.ExtraTypes exposing(..)"]
                    ,   T.unlines imports
                    , ""
                    , "-- the types of all places in the net"
                    , generateNetTypes name places -- the initial places
                    , "type Transition ="] ++
                    (if length internalClientTransitions > 0 then
                        ["      Internal InternalTransition |"] else []) ++
                    [ "    External OutgoingTransition"
                    , "  |  NoOp"
                    ]
                transFromPlace :: T.Text -> [Transition] -> [(Bool,Transition)]
                transFromPlace place =
                        mapMaybe (\tr ->
                                case tr of
                                    Transition tt _ lstTrans _->
                                        if tt /= OriginServerOnly && any (\(from,_) -> from == place) lstTrans then Just (False,tr) else Nothing
                                    ClientTransition _ pl -> if pl == place then Just (True,tr) else Nothing
                                    CmdTransition _ pl _ -> if pl == place then Just (False,tr) else Nothing
                            )
                perPlaceTypes (Place placeName _ _ _ _) = 
                    let
                        placeTranss = transFromPlace placeName transitions
                        transConstrs = map (trans2constr . snd) placeTranss
                        imports = concatMap (findImports Elm) $ concatMap snd transConstrs

                    in
                    T.unlines
                    [
                        T.concat["module ",name,".Static.Types.",placeName," exposing(..)"]
                    ,   T.concat["import ",name,".Static.ExtraTypes exposing(..)"]
                    ,   T.unlines imports
                    ,   if length transConstrs == 0 then "x = 0" else ""
                    ,   generateType Elm False True [] $ ct "Msg" transConstrs
                    ]
                perPlaceWrappers (Place placeName _ _ _ _) = 
                    let
                        placeTranss = transFromPlace placeName transitions
                        transConstrs = 
                            map (\(n,ets) -> 
                                (T.concat[name,".Static.Types.",placeName,".",n],ets)) $ 
                                map (trans2constr . snd) placeTranss
                        wrappedTransConstrs = map (\(n,t) -> (T.concat["T",n],t)) $ map (trans2constr . snd) placeTranss
                        internalTrans = map fst placeTranss
                        zippedTrans = zip3 transConstrs wrappedTransConstrs internalTrans
                        imports = concatMap (findImports Elm) $ concatMap snd wrappedTransConstrs
                    in
                    T.unlines
                    [
                        T.concat["module ",name,".Static.Wrappers.",placeName," exposing(..)"]
                    ,   T.concat["import ",name,".Static.Types.",placeName," exposing(..)"]
                    ,   T.concat["import ",name,".Static.ExtraTypes exposing(..)"]
                    ,   T.concat["import ",name,".Static.Types exposing(..)"]
                    ,   T.unlines imports
                    ,   "import Dict exposing (Dict)"
                    ,   ""
                    ,   "unwrap : Msg -> Transition"
                    ,   "unwrap msg ="
                    ,   "    case msg of"
                    ,   if length zippedTrans == 0 then
                          T.concat ["        ",name,".Static.Types.",placeName,".Msg -> NoOp"]
                        else ""
                    ,   T.unlines $ map (\(tc,wtc,int) -> T.concat["        ",generatePattern tc," -> ",if int then "Internal " else "External ",generatePattern wtc]) zippedTrans
                    ]
                perPlaceViews (Place placeName _ _ _ _) = 
                    T.unlines
                    [
                        T.concat["module ",name,".View.",placeName," exposing(..)"]
                    ,   T.concat["import ",name,".Static.Types.",placeName," exposing(Msg(..))"]
                    ,   T.concat["import ",name,".Static.Types exposing(",placeName,"(..))"]
                    ,   T.concat["import ",name,".Static.Helpers.",placeName," exposing(..)"]
                    ,   T.concat ["import ",name,".Static.ExtraTypes exposing(..)\n"]
                    ,   "import Html exposing(Html)"
                    ,   "import Debug exposing(todo)"
                    ,   ""
                    ,   T.concat["subs : ", placeName, " -> Sub Msg"]
                    ,   T.concat["subs ", uncapitalize placeName, " ="]
                    ,   "    Sub.none"
                    ,   ""
                    ,   T.concat["view : ", placeName, " -> Html Msg"]
                    ,   T.concat["view ", uncapitalize placeName, " ="]
                    ,   T.concat["    todo \"Please fill out the view function for the ",name," net for the ",placeName," place.\""]
                    ,   ""
                    ,   T.concat["title : ", placeName, " -> String"]
                    ,   T.concat["title ", uncapitalize placeName, " ="]
                    ,   T.concat["    todo \"Please fill out the title function for the ",name," net for the ",placeName," place.\""]
                    ]
                fromSuperPlace = 
                    T.unlines
                    [
                        T.concat["module ",name,".Static.FromSuperPlace exposing(..)"]
                    ,   "import Static.Types exposing(TopLevelData)"
                    ,   "type alias FromSuperPlace = TopLevelData" --FIXME: change this depending on where the net resides
                    ]
                incomingMsgs :: [(T.Text,Constructor)]
                incomingMsgs = concat $ map (\tr -> case tr of 
                                                        Transition _ (n,_) lstTrans _ -> 
                                                            mapMaybe (\(from,mTo) -> case mTo of 
                                                                                        Just (to,msg) -> Just (n,msg)
                                                                                        Nothing -> Nothing)
                                                                                            lstTrans
                                                        ClientTransition constr@(n, _) _ -> [(n,constr)] 
                                                        CmdTransition constr@(n, _) _ _ -> [(n,constr)] 
                                                        ) transitions
                incomingCM = map (\(_,(msgN,edts)) -> (msgN,edts)) incomingMsgs
                incomingMsg = CustomT "IncomingMessage" $ map (\(n,t) -> (T.concat["M",n],t)) incomingCM

                transConstrs :: [Constructor]
                transConstrs = map trans2constr transitions

                outgoingFromClient localExcluded t = 
                    case t of 
                        Transition OriginClientOnly _ _ _ -> True
                        Transition OriginEitherPossible _ _ _ -> True
                        ClientTransition {} -> not localExcluded
                        CmdTransition _ _ _ -> True
                        _ -> False

                internalClientTransitions =
                    filter internalToClient transitions
                
                internalToClient t = 
                    case t of 
                        ClientTransition {} -> True
                        _ -> False

                outgoingClientTransitions localExcluded = filter (outgoingFromClient localExcluded) transitions
                
                singletonTypes = filter (\(CustomT _ constrs) -> length constrs == 1) $ M.elems extraTypes
                        
                internalMap =
                    M.fromList $
                           map (\trans -> (getTransitionName trans, True)) internalClientTransitions
                        ++ map (\trans -> (getTransitionName trans, False)) (outgoingClientTransitions True)

                generateNetTypes :: T.Text -> [Place] -> T.Text
                generateNetTypes netName places = 
                    let
                        placeModel = 
                            generateType Elm False True [DOrd,DEq,DShow] $
                                CustomT netName $ map (\(Place n _ _ _ _) -> (n,[dt (TypeT n) "" ""])) places
                        placeTypes = T.unlines $ map generatePlaceType places
                        generatePlaceType :: Place -> T.Text
                        generatePlaceType (Place name _ _ clientPlaceState _) =
                            T.unlines
                                [
                                    generateType Elm True True [DOrd,DEq,DShow,DTypeable] $ CustomT name [(name, clientPlaceState)],""
                                ]
                        clientMsgType :: T.Text
                        clientMsgType =
                            let
                                
                            in
                                generateType Elm True True [DOrd,DEq,DShow] incomingMsg

                        individualMessages =
                            fnub $
                                map (\(_,msg@(msgN,edts)) -> ct msgN [msg]) incomingMsgs ++
                                map (\(pl,etd) -> ct pl [(pl,etd)]) (map trans2constr (outgoingClientTransitions False))


                        --allConnections = concat $ map (\(_, Transition (transName,transEt) connections mCmd) -> ((transName,transEt),connections)) transitions

                        internalTransitions = ct "InternalTransition" $ map (\(n,t) -> (T.concat["T",n],t)) $ map trans2constr internalClientTransitions
                        outgoingTransitions = ct "OutgoingTransition" $ map (\(n,t) -> (T.concat["T",n],t)) $ map trans2constr (outgoingClientTransitions True)
                    in
                        T.unlines 
                            [
                                "-- place states"
                            ,   placeTypes
                            ,   "-- union place type"
                            ,   generateType Elm False True [] $ ct "NetState" $ map (\placeName -> constructor (T.concat["S",placeName]) [dt (TypeT placeName) "" ""]) placeNames
                            ,   "-- internal transition types"
                            ,   generateType Elm False True [] internalTransitions
                            ,   "-- outgoing transition types"
                            ,   generateType Elm False True [] outgoingTransitions
                            ,   T.unlines $ map (generateType Elm False True []) individualMessages
                            ,   "-- outgoing server message types"
                            ,   clientMsgType
                            ]

                hiddenExtraTypes = 
                    let
                        imports = concatMap (findImports Elm) $ concatMap (\(CustomT _ edts) -> concatMap snd edts) $ M.elems extraTypes
                    in
                    T.unlines
                    [
                        T.concat ["module ",name,".Static.ExtraTypes exposing(..)"]
                    ,   T.unlines imports
                    ,   "-- extra client types"
                    ,   T.unlines $ map (generateType Elm True True [DOrd,DEq,DShow] . snd) $ M.toList extraTypes
                    ,   if length extraTypes == 0 then "x = 0" else ""
                    ]
                
                update :: T.Text
                update =
                    let
                        trUpFn :: Transition -> T.Text
                        trUpFn (Transition _ (transName,_) connections _) =
                            let
                                conUpFn (from,mTo) = 
                                    case mTo of
                                        Just (to,(n,et)) -> 
                                            let
                                                fnName = T.concat["update",from,n,to]
                                            in Just $ 
                                            T.unlines
                                            [T.concat[fnName," : FromSuperPlace -> ",n," -> ",from," -> (",to,", Cmd ",to,"T.Msg)"]
                                            ,T.concat[fnName," fsp ",generatePattern (n,et)," ",uncapitalize from," ="]
                                            ,T.concat["    todo \"Please implement update function ",fnName," for the ",name," net.\""]
                                            ]
                                        _ -> Nothing
                            in
                            T.unlines $ mapMaybe conUpFn connections 
                        trUpFn (ClientTransition (n,et) place) =                         
                            let
                                fnName = T.concat["update",n,place]
                            in
                                T.unlines
                                    [T.concat[fnName," : FromSuperPlace -> ",n," -> ",place," -> (",place,", Cmd ",place,"T.Msg)"]
                                    ,T.concat[fnName," fsp ",generatePattern (n,et)," ",uncapitalize place," ="]
                                    ,T.concat["    (todo \"Please implement update function ",fnName," for the ",name," net.\", Cmd.none)"]
                                    ]
                        trUpFn _ = ""
                    in T.unlines
                    [
                        T.concat ["module ",name,".Update exposing(..)"]
                    ,   T.concat ["import ",name,".Static.Types exposing(..)"]
                    ,   T.unlines $ map (\placeName -> T.concat ["import ",name,".Static.Types.",placeName," as ",placeName,"T"]) placeNames
                    ,   T.concat ["import ",name,".Static.FromSuperPlace exposing(..)"]
                    ,   T.concat ["import ",name,".Static.ExtraTypes exposing(..)"]
                    ,   T.intercalate "\n" $ map (\placeName -> T.concat ["import ",name,".Static.Helpers.",placeName," as ",placeName]) placeNames
                    ,   "import Utils.Utils"
                    ,   "import Debug exposing(todo)"
                    ,   ""
                    ,   T.unlines $ map trUpFn transitions
                    ]
                {-fromsTos :: (HybridTransition, Transition) -> [(T.Text,T.Text)]
                fromsTos (_, Transition (transName,_) connections mCmd) =
                    fnub $ map (\(from,(to,_)) -> (from,to)) connections-}
                fromsTos :: Transition -> ([T.Text],[T.Text])
                fromsTos (Transition _ (transName,_) connections mCmd) =
                    (fnub $ map (\(from,_) -> from) connections,fnub $ mapMaybe (\(from,mTo) -> if isJust mTo then fmap fst mTo else Just from) connections)
                fromsTos (ClientTransition _ place) =
                    ([place],[place])
                fromsTos _ =
                    ([],[])
                hiddenUpdate :: T.Text
                hiddenUpdate = 
                    let
                        updateCase :: Transition -> T.Text
                        updateCase (Transition _ (transName,_) connections _) =
                            let
                                connectionCase (from,mTo) =
                                    case mTo of
                                        Just (to,(n,t)) -> 
                                            Just $ T.concat
                                                ["        ("
                                                ,generatePattern (T.concat["M",n], t)
                                                ,", S",from," st)"
                                                ," -> let (newModel, cmd) = update",from,n,to
                                                ," fsp ",generatePattern (n, t)," st in (S",to," newModel, Cmd.map ",name,".Static.Wrappers.",to,".unwrap cmd)"
                                                ]
                                        _ -> Nothing
                            in
                                T.unlines $ mapMaybe connectionCase connections
                        updateCase (ClientTransition (n,t) place) =
                            T.concat ["        ("
                                ,generatePattern (T.concat["M",n],t)
                                ,", S",place," st)"
                                ," -> Tuple.mapBoth S",place," (Cmd.map ",name,".Static.Wrappers.",place,".unwrap)"
                                ," <| update", n,place
                                ," fsp ",generatePattern (n,t)," st"
                                ]
                        updateCase (CmdTransition {}) =
                            ""
                        ttCase :: Transition -> Maybe T.Text
                        ttCase (Transition tt (name,ets) _ _) = 
                            if tt == OriginClientOnly || tt == OriginEitherPossible then Just $ T.concat["        ",generatePattern (T.concat ["T",name],ets)," -> OutgoingToServer"] else Nothing
                        ttCase (ClientTransition (name,ets) _) =
                            Just $ T.concat["        ",generatePattern (T.concat ["T",name],ets)," -> LocalOnly"]
                        ttCase (CmdTransition (name,ets) _ _) =
                            Just $ T.concat["        ",generatePattern (T.concat ["T",name],ets)," -> OutgoingToServer"]
                        o2i :: Transition -> Maybe T.Text
                        o2i (Transition origin (name,ets) _ _) = 
                            Nothing--if origin == OriginServerOnly then Nothing
                            --else Just $ T.concat["        External outT -> Right outT"]
                        o2i (CmdTransition (name,ets) _ _) = 
                            Nothing--Just $ T.concat["        External outT -> Right outT"]
                        o2i (ClientTransition (name,ets) _) =
                            Just $ T.concat["        Internal ",generatePattern (T.concat ["T",name],ets)," -> Left <| ",generatePattern (T.concat ["M",name],ets)]
                    in T.unlines
                    [
                        T.concat ["module ",name,".Static.Update exposing(..)"]
                    ,   T.concat ["import ",name,".Static.Types exposing(..)"]
                    ,   T.concat ["import ",name,".Static.Wrappers exposing(..)"]
                    ,   T.unlines $ map (\place -> T.concat ["import ",name,".Static.Wrappers.",place," exposing(..)"]) placeNames
                    ,   T.concat ["import ",name,".Static.FromSuperPlace exposing (FromSuperPlace)"]
                    ,   T.concat ["import ",name,".Update exposing(..)"]
                    ,   "import Utils.Utils exposing(Either(..))"
                    ,   "import Static.Types exposing(..)"
                    ,   "import Dict"
                    ,   ""
                    ,   T.concat ["update : FromSuperPlace -> IncomingMessage -> NetState -> (NetState,Cmd Transition)"]
                    ,   T.concat ["update fsp trans state ="]
                    ,   "    case (trans,state) of"
                    ,   T.unlines $ map updateCase transitions
                    ,   if length places > 1 then "        _ -> (state, Cmd.none)" else ""
                   {-} ,   "transitionType : OutgoingTransition -> TransitionType"
                    ,   "transitionType oTrans ="
                    ,   "    case oTrans of"
                    ,   T.unlines $ mapMaybe ttCase transitions-}
                    ,   "outgoingToIncoming : Transition -> Maybe (Either IncomingMessage Transition)"
                    ,   "outgoingToIncoming trans ="
                    ,   "    case trans of"
                    ,   T.unlines $ mapMaybe o2i transitions --internalClientTransitions
                    ,   "        External outT -> Just <| Right <| External outT"
                    ,   "        NoOp -> Nothing"
                    --,   if length (mapMaybe o2i transitions) < length transitions then
                    --        "        _ -> Nothing"
                    --    else ""
                    ]
            
                placeMap :: M.Map T.Text Place
                placeMap = M.fromList $ map (\(pl@(Place n _ _ _ _)) -> (n,pl)) places

                getPlace :: T.Text -> Place
                getPlace name = M.findWithDefault (Place "" [] [] [] Nothing) name placeMap

                hiddenInit = T.unlines
                    [
                        T.concat ["module ",name,".Static.Init exposing(..)"]
                    ,   T.concat ["import ",name,".Init as Init"]
                    ,   T.concat ["import ",name,".Static.Types exposing (NetState(..))"]
                    ,   T.concat ["import ",name,".Update as Update"]
                    ,   T.concat ["import ",name,".Static.Wrappers"]
                    ,   T.concat ["import ",name,".Static.ExtraTypes exposing(..)\n"]
                    ,   "init : NetState"
                    ,   T.concat ["init = S",startingPlace," Init.init"]
                    ]
                encoder =
                    T.unlines $
                    [
                        T.concat ["module ",name,".Static.Encode exposing(..)"]
                    ,   T.concat ["import ",name,".Static.Types exposing(..)"]
                    ,   T.concat ["import ",name,".Static.ExtraTypes exposing(..)\n"]
                    ,   "import Utils.Utils exposing(..)"
                    ,   "import Static.Types"
                    ,   "import Dict exposing(..)"
                    ,   "encodeTransition : Transition -> Maybe String"
                    ,   "encodeTransition trans ="
                    ,   "    case trans of"] ++
                    (if length internalClientTransitions > 0 then ["        Internal _ -> Nothing"] else [""]) ++
                    [   "        External ext -> Just <| encodeOutgoingTransition ext"
                    ,   "        NoOp -> Nothing"
                    ,   generateEncoder Elm outgoingTransitions
                    ,   "--extra types encoders"
                    ,   T.unlines $ map (generateEncoder Elm) $ M.elems extraTypes
                    ]

                wOutgoingClientTransitions = map (\(n,t) -> (T.concat["T",n],t)) $ map trans2constr (outgoingClientTransitions True)
                outgoingTransitions = CustomT "OutgoingTransition" wOutgoingClientTransitions
                incomingClientTransitions localOnly = 
                    concat $ mapMaybe (\tr ->
                        case tr of 
                            Transition tt (name,ets) clientTransitions _ -> 
                                Just $ mapMaybe (\(from,mTo) -> fmap snd mTo) clientTransitions 
                            ClientTransition constr _ ->
                                if localOnly then Nothing else Just [constr]
                            CmdTransition constr _ _ ->
                                Just [constr]) transitions
                incomingTransitions localOnly = 
                    CustomT "IncomingMessage" $ map (\(n,et) -> (T.concat ["M",n],et)) (incomingClientTransitions localOnly)
                decoder = T.unlines 
                    [
                        T.concat ["module ",name,".Static.Decode exposing(..)"]
                    ,   T.concat ["import ",name,".Static.Types exposing(..)"]
                    ,   T.concat ["import ",name,".Static.ExtraTypes exposing(..)\n"]
                    ,   "import Utils.Utils exposing(..)"
                    ,   generateDecoder Elm $ incomingTransitions True
                    ,   "--extra types decoders"
                    ,   T.unlines $ map (generateDecoder Elm) $ M.elems extraTypes
                    ]
                wrappers = 
                    T.unlines
                        [
                            T.concat ["module ",name,".Static.Wrappers exposing(..)"]
                        ,   T.concat["import ",name,".Static.ExtraTypes exposing(..)"]
                        ,   T.concat ["import ",name,".Static.Types exposing(..)\n"]
                        ,   "import Dict exposing (Dict)"
                        ,   "dummyValue = 0"
                       -- ,   T.unlines $ map (createWrap extraTypes (length places > 1) Elm "IncomingMessage" "M") incomingCM
                       ,   T.unlines $ map (createUnwrap Elm "OutgoingTransition" "T") $ map trans2constr $ outgoingClientTransitions True
                       ,   T.unlines $ map (createUnwrap Elm "InternalTransition" "T") $ map trans2constr internalClientTransitions
                       ]
                hiddenView = 
                    let
                        viewCase placeName = T.concat["        S",placeName," m -> Html.map ",name,".Static.Wrappers.",placeName,".unwrap <| ",name,".View.",placeName,".view m"]
                        titlCase placeName = T.concat["        S",placeName," m -> ", name,".View.",placeName,".title m"]
                    in
                    T.unlines
                    [
                        T.concat["module ",name,".Static.View exposing(..)"]
                    ,   "import Html exposing(Html)"
                    ,   "import Static.Types exposing (NetModel)"
                    ,   T.concat["import ",name,".Static.Types exposing(..)"]
                    ,   T.unlines $ map (\placeName -> T.concat["import ",name,".View.",placeName]) placeNames
                    ,   T.unlines $ map (\placeName -> T.concat["import ",name,".Static.Wrappers.",placeName]) placeNames
                    ,   ""
                    ,   "view : NetState -> Html Transition"
                    ,   "view ns ="
                    ,   "    case ns of"
                    ,   T.unlines $ map viewCase placeNames
                    ,   "title : NetState -> String"
                    ,   "title ns ="
                    ,   "    case ns of"
                    ,   T.unlines $ map titlCase placeNames
                    ]

                hiddenSubs =
                    let
                        subCase placeName =
                            T.concat ["        S",placeName," m -> Sub.map ",name,".Static.Wrappers.",placeName,".unwrap"," <| ",placeName,".subs m"]
                    in
                    T.unlines
                    [
                        T.concat["module ",name,".Static.Subs exposing(..)"]
                    ,   T.concat["import ",name,".Static.Types exposing(..)"]
                    --,   T.unlines $ map (\(plName,_) -> T.concat["import ",name,".",plName,".Update as ",plName," exposing(subs)"]) placesWithSubs
                    ,   T.unlines $ map (\placeName -> T.concat["import ",name,".View.",placeName," as ",placeName]) placeNames
                    ,   T.unlines $ map (\placeName -> T.concat["import ",name,".Static.Wrappers.",placeName]) placeNames
                    ,   ""
                    ,   "subs : NetState -> Sub Transition"
                    ,   "subs model ="
                    ,   "    case model of"
                    ,   T.unlines $ map subCase placeNames
                    ]
            in do
                --user code
                createDirectoryIfMissing True $ fp </> "client" </> "src" </> T.unpack name </> "View"
                mapM_ (\pl -> writeIfNotExists (fp </> "client" </> "src" </> T.unpack name </> "View" </> T.unpack (getPlaceName pl) <.> "elm") $ perPlaceViews pl) places
                writeIfNotExists (fp </> "client" </> "src" </> T.unpack name </> "Init" <.> "elm") inits 
                writeIfNotExists (fp </> "client" </> "src" </> T.unpack name </> "Update" <.> "elm") update

                --template directory (always generated)
                createDirectoryIfMissing True $ fp </> "client" </> "src" </> T.unpack name </> "Templates" </> "View"
                mapM_ (\pl -> writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Templates" </> "View" </> T.unpack (getPlaceName pl) <.> "elm") $ perPlaceViews pl) places
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Templates" </> "Init" <.> "elm") inits 
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Templates" </> "Update" <.> "elm") update
                
                --static / rewriteable code
                createDirectoryIfMissing True $ fp </> "client" </> "src" </> T.unpack name </> "Static"
                createDirectoryIfMissing True $ fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Types"
                mapM_ (\pl -> writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Types" </> T.unpack (getPlaceName pl) <.> "elm") $ perPlaceTypes pl) places
                createDirectoryIfMissing True $ fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Wrappers"
                mapM_ (\pl -> writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Wrappers" </> T.unpack (getPlaceName pl) <.> "elm") $ perPlaceWrappers pl) places
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Types" <.> "elm") types
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Init" <.> "elm") hiddenInit
                createDirectoryIfMissing True $ fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Helpers"
                
                mapM_ (\(Place pName _ _ edts _)  -> writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Helpers" </> T.unpack pName <.> "elm") $ T.unlines $ {-disclaimer currentTime :-} [generateHelper Elm pName name (pName,edts) False]) places
                mapM_ (\(CustomT cn constrs) -> writeIfNew 1 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Helpers" </> T.unpack cn <.> "elm") $ T.unlines [generateHelper Elm cn name (head constrs) False]) singletonTypes
                
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Encode" <.> "elm") encoder
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Decode" <.> "elm") decoder
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Wrappers" <.> "elm") wrappers
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Update" <.> "elm") hiddenUpdate
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "FromSuperPlace" <.> "elm") fromSuperPlace
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "View" <.> "elm") hiddenView
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "ExtraTypes" <.> "elm") hiddenExtraTypes
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Subs" <.> "elm") hiddenSubs
                createDirectoryIfMissing True $ fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Standalone"
                generateStandalones extraTypes name fp places
