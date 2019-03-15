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

trans2constr :: NetTransition -> Constructor
trans2constr trans = 
    case trans of
        NetTransition tt constr _ _ -> constr
        ClientTransition constr _ _ -> constr
        CmdTransition constr _ _    -> constr


place2edts :: HybridPlace -> [ElmDocType]
place2edts (HybridPlace _ _ _ edts _ _) = edts
    

transName from msgN = T.concat [T.pack msgN,"from",from]

netTrans2MConstr :: NetTransition -> Maybe Constructor
netTrans2MConstr trans = 
    case trans of
        NetTransition tt constr _ _ -> if tt == OriginClientOnly || tt == OriginEitherPossible then Just constr else Nothing
        CmdTransition constr _ _-> Just constr
        ClientTransition {} -> Nothing


generate :: M.Map String ElmCustom -> FilePath -> Net -> IO ()
generate extraTypes fp net =
    case net of 
        (HybridNet name startingPlace places transitions plugins) ->
            let
                inits = T.unlines 
                    [
                    T.concat ["module ", name, ".Init exposing(..)"]
                    ,T.concat["import ",name,".Static.Types exposing(..)"]
                    , ""
                    , "-- the initial states of each place in this net"
                    , T.concat ["init : ",startingPlace]
                    , T.concat ["init = ",constr2Def extraTypes (getPlaceState $ getPlace startingPlace)]
                    ]
                placeNames = map (\(HybridPlace name _ _ _ _ _) -> name) places
                -- the functions that the user changes
                generateNetInit :: M.Map String ElmCustom -> HybridPlace -> T.Text
                generateNetInit extraTypes (HybridPlace name _ _ clientPlaceState mSubnet (mCmd,_)) = 
                    let
                        fnName = T.concat ["init",name]
                        typ = case mCmd of
                                Just cmdN -> T.concat [fnName, " : (", capitalize name,", Cmd ",cmdN,")"]
                                Nothing       -> T.concat [fnName, " : ", capitalize name]
                        decl = case mCmd of
                                Just _  -> T.concat [fnName, " = (", constr2Def extraTypes (T.unpack name,clientPlaceState),", Cmd.none)"]
                                Nothing -> T.concat [fnName, " = ", constr2Def extraTypes (T.unpack name,clientPlaceState)]
                    in
                        T.unlines 
                        [
                            typ
                        ,   decl
                        ]
                types = 
                    let
                        transConstrs = concatMap (\(_,ets) -> ets) $ map trans2constr $ outgoingClientTransitions True
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
                    , "type Dummy = Dummy"
                    , generateNetTypes name places -- the initial places
                    , "type Transition ="] ++
                    (if length internalClientTransitions > 0 then 
                        ["      Internal InternalTransition |"] else []) ++
                    [ "    External OutgoingTransition"
                    ]
                transFromPlace :: T.Text -> [(Bool,NetTransition)]
                transFromPlace place = 
                        mapMaybe (\tr ->
                                case tr of
                                    NetTransition tt _ lstTrans _->
                                        if tt /= OriginServerOnly && any (\(from,_) -> from == place) lstTrans then Just (False,tr) else Nothing
                                    ClientTransition _ pl _ -> if pl == place then Just (True,tr) else Nothing
                                    CmdTransition _ pl _ -> if pl == place then Just (False,tr) else Nothing
                            ) transitions
                perPlaceTypes (HybridPlace placeName _ _ _ _ _) = 
                    let
                        transitions = transFromPlace placeName
                        transConstrs = map (trans2constr . snd) transitions
                        imports = concatMap (findImports Elm) $ concatMap snd transConstrs

                    in
                    T.unlines
                    [
                        T.concat["module ",name,".Static.Types.",placeName," exposing(..)"]
                    ,   T.concat["import ",name,".Static.ExtraTypes exposing(..)"]
                    ,   T.unlines imports
                    ,   if length transConstrs == 0 then "x = 0" else ""
                    ,   generateType Elm False [] $ ec "Msg" transConstrs
                    ]
                perPlaceWrappers (HybridPlace placeName _ _ _ _ _) = 
                    let
                        transitions = transFromPlace placeName
                        transConstrs = 
                            map (\(n,ets) -> 
                                (T.unpack name++".Static.Types."++T.unpack placeName++"."++n,ets)) $ 
                                map (trans2constr . snd) transitions
                        wrappedTransConstrs = map (\(n,t) -> ("T"++n,t)) $ map (trans2constr . snd) transitions
                        internalTrans = map fst transitions
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
                    ,   T.unlines $ map (\(tc,wtc,int) -> T.concat["        ",generatePattern tc," -> ",if int then "Internal " else "External ",generatePattern wtc]) zippedTrans
                    ]
                perPlaceViews (HybridPlace placeName _ _ _ _ _) = 
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
                incomingMsgs :: [(String,Constructor)]
                incomingMsgs = concat $ map (\tr -> case tr of 
                                                        NetTransition _ (n,_) lstTrans _ -> 
                                                            mapMaybe (\(from,mTo) -> case mTo of 
                                                                                        Just (to,msg) -> Just (n,msg)
                                                                                        Nothing -> Nothing)
                                                                                            lstTrans
                                                        ClientTransition constr@(n, _) _ _ -> [(n,constr)] 
                                                        CmdTransition constr@(n, _) _ _ -> [(n,constr)] 
                                                        ) transitions
                incomingCM = map (\(_,(msgN,edts)) -> (msgN,edts)) incomingMsgs
                incomingMsg = ElmCustom "IncomingMessage" $ map (\(n,t) -> ("M"++n,t)) incomingCM

                transConstrs :: [Constructor]
                transConstrs = map trans2constr transitions

                outgoingFromClient localExcluded t = 
                    case t of 
                        NetTransition OriginClientOnly _ _ _ -> True
                        NetTransition OriginEitherPossible _ _ _ -> True
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

                internalMap =
                    M.fromList $
                           map (\trans -> (getTransitionName trans, True)) internalClientTransitions
                        ++ map (\trans -> (getTransitionName trans, False)) (outgoingClientTransitions True)

                generateNetTypes :: T.Text -> [HybridPlace] -> T.Text
                generateNetTypes netName places = 
                    let
                        placeModel = 
                            generateType Elm False [DOrd,DEq,DShow] $ 
                                ElmCustom (T.unpack netName) $ map (\(HybridPlace n _ _ _ _ _) -> (T.unpack n,[edt (ElmType $ T.unpack n) "" ""])) places
                        placeTypes = T.unlines $ map generatePlaceType places
                        generatePlaceType :: HybridPlace -> T.Text
                        generatePlaceType (HybridPlace name _ _ clientPlaceState _ _) =
                            T.unlines
                                [
                                    generateType Elm True [DOrd,DEq,DShow,DTypeable] $ ElmCustom (T.unpack name) [(T.unpack name, clientPlaceState)],""
                                ]
                        clientMsgType :: T.Text
                        clientMsgType =
                            let
                                
                            in
                                generateType Elm True [DOrd,DEq,DShow] incomingMsg

                        individualMessages =
                            fnub $
                                map (\(_,msg@(msgN,edts)) -> ec msgN [msg]) incomingMsgs ++
                                map (\(pl,etd) -> ec pl [(pl,etd)]) (map trans2constr (outgoingClientTransitions False))


                        --allConnections = concat $ map (\(_, NetTransition (transName,transEt) connections mCmd) -> ((transName,transEt),connections)) transitions

                        internalTransitions = ec "InternalTransition" $ map (\(n,t) -> ("T"++n,t)) $ map trans2constr internalClientTransitions
                        outgoingTransitions = ec "OutgoingTransition" $ map (\(n,t) -> ("T"++n,t)) $ map trans2constr (outgoingClientTransitions True)
                    in
                        T.unlines 
                            [
                                "-- place states"
                            ,   placeTypes
                            ,   "-- union place type"
                            ,   generateType Elm False [] $ ec "NetState" $ map (\placeName -> constructor ("S"++T.unpack placeName) [edt (ElmType $ T.unpack placeName) "" ""]) placeNames
                            ,   "-- internal transition types"
                            ,   generateType Elm False [] internalTransitions
                            ,   "-- outgoing transition types"
                            ,   generateType Elm False [] outgoingTransitions
                            ,   T.unlines $ map (generateType Elm False []) individualMessages 
                            ,   "-- outgoing server message types"
                            ,   clientMsgType
                            ]

                hiddenExtraTypes = 
                    T.unlines
                    [
                        T.concat ["module ",name,".Static.ExtraTypes exposing(..)"]
                    ,   "-- extra client types"
                    ,   T.unlines $ map (generateType Elm True [DOrd,DEq,DShow] . snd) $ M.toList extraTypes
                    ,   if length extraTypes == 0 then "x = 0" else ""
                    ]
                
                update :: T.Text
                update =
                    let
                        trUpFn :: NetTransition -> T.Text
                        trUpFn (NetTransition _ (transName,_) connections _) =
                            let
                                conUpFn (from,mTo) = 
                                    case mTo of
                                        Just (to,(n,et)) -> 
                                            let
                                                fnName = T.concat["update",from,T.pack n,to]
                                            in Just $ 
                                            T.unlines
                                            [T.concat[fnName," : FromSuperPlace -> ",T.pack n," -> ",from," -> ",to]
                                            ,T.concat[fnName," fsp ",generatePattern (n,et)," ",uncapitalize from," ="]
                                            ,T.concat["    todo \"Please implement update function ",fnName," for the ",name," net.\""]
                                            ]
                                        _ -> Nothing
                            in
                            T.unlines $ mapMaybe conUpFn connections 
                        trUpFn (ClientTransition (n,et) place mCmd) =                         
                            let
                                fnName = T.concat["update",T.pack n,place]
                            in
                            case mCmd of
                                Just cmd ->
                                    T.unlines
                                    [T.concat[fnName," : FromSuperPlace -> ",T.pack n," -> ",place," -> (",place,", Cmd ",cmd,")"]
                                    ,T.concat[fnName," fsp ",generatePattern (n,et)," ",uncapitalize place," ="]
                                    ,T.concat["    (todo \"Please implement update function ",fnName," for the ",name," net.\", Cmd.none)"]
                                    ]
                                Nothing ->
                                    T.unlines
                                    [T.concat[fnName," : FromSuperPlace -> ",T.pack n," -> ",place," -> ",place]
                                    ,T.concat[fnName," fsp ",generatePattern (n,et)," ",uncapitalize place," ="]
                                    ,T.concat["    todo \"Please implement update function ",fnName," for the ",name," net.\""]
                                    ]
                        trUpFn _ = ""
                    in T.unlines
                    [
                        T.concat ["module ",name,".Update exposing(..)"]
                    ,   T.concat ["import ",name,".Static.Types exposing(..)"]
                    ,   T.concat ["import ",name,".Static.FromSuperPlace exposing(..)"]
                    ,   T.concat ["import ",name,".Static.ExtraTypes exposing(..)"]
                    ,   T.intercalate "\n" $ map (\placeName -> T.concat ["import ",name,".Static.Helpers.",placeName," as ",placeName]) placeNames
                    ,   "import Utils.Utils"
                    ,   "import Debug exposing(todo)"
                    ,   ""
                    ,   T.unlines $ map trUpFn transitions
                    ]
                {-fromsTos :: (HybridTransition, NetTransition) -> [(T.Text,T.Text)]
                fromsTos (_, NetTransition (transName,_) connections mCmd) =
                    fnub $ map (\(from,(to,_)) -> (from,to)) connections-}
                fromsTos :: NetTransition -> ([T.Text],[T.Text])
                fromsTos (NetTransition _ (transName,_) connections mCmd) =
                    (fnub $ map (\(from,_) -> from) connections,fnub $ mapMaybe (\(from,mTo) -> if isJust mTo then fmap fst mTo else Just from) connections)
                fromsTos (ClientTransition _ place _) =
                    ([place],[place])
                fromsTos _ =
                    ([],[])
                hiddenUpdate :: T.Text
                hiddenUpdate = 
                    let
                        updateCase :: NetTransition -> T.Text
                        updateCase (NetTransition _ (transName,_) connections _) =
                            let
                                connectionCase (from,mTo) =
                                    case mTo of 
                                        Just (to,(n,t)) -> Just $ T.concat ["        ("
                                                                      ,generatePattern ("M"++n, t)
                                                                      ,", S",from," st)"
                                                                      ," -> (S",to
                                                                      ," <| update",from,T.pack n,to
                                                                      ," fsp ",generatePattern (n, t)," st, Cmd.none)"
                                                                      ]
                                        _ -> Nothing
                            in
                                T.unlines $ mapMaybe connectionCase connections
                        updateCase (ClientTransition (n,t) place mCmd) =
                            case mCmd of 
                                Just cmd ->
                                    let
                                        cmdInternal = 
                                            case M.lookup cmd internalMap of
                                                Just it -> it
                                                _ -> error $ "Cmd " ++ T.unpack cmd ++ " does not exist!"
                                    in
                                    T.concat ["        ("
                                        ,generatePattern ("M"++n,t)
                                        ,", S",place," st)"
                                        ," -> Tuple.mapBoth S",place," (Cmd.map <|"
                                        ,if cmdInternal then " Internal " else " External " ," << unwrap",cmd,")"
                                        ," <| update",T.pack n,place
                                        ," fsp ",generatePattern (n,t)," st"
                                        ]
                                Nothing ->
                                    T.concat ["        ("
                                        ,generatePattern ("M"++n,t)
                                        ,", S",place," st)"
                                        ," -> (S",place
                                        ," <| update",T.pack n,place
                                        ," fsp ",generatePattern (n,t)," st, Cmd.none)"
                                        ]
                        updateCase (CmdTransition {}) =
                            ""
                        ttCase :: NetTransition -> Maybe T.Text
                        ttCase (NetTransition tt (name,ets) _ _) = 
                            if tt == OriginClientOnly || tt == OriginEitherPossible then Just $ T.concat["        ",generatePattern ("T"++name,ets)," -> OutgoingToServer"] else Nothing
                        ttCase (ClientTransition (name,ets) _ _) =
                            Just $ T.concat["        ",generatePattern ("T"++name,ets)," -> LocalOnly"]
                        ttCase (CmdTransition (name,ets) _ _) =
                            Just $ T.concat["        ",generatePattern ("T"++name,ets)," -> OutgoingToServer"]
                        o2i :: NetTransition -> Maybe T.Text
                        o2i (NetTransition _ (name,ets) _ _) = 
                            Nothing
                        o2i (CmdTransition (name,ets) _ _) = 
                            Nothing
                        o2i (ClientTransition (name,ets) _ _) =
                            Just $ T.concat["        Internal ",generatePattern ("T"++name,ets)," -> Just ",generatePattern ("M"++name,ets)]
                    in T.unlines
                    [
                        T.concat ["module ",name,".Static.Update exposing(..)"]
                    ,   T.concat ["import ",name,".Static.Types exposing(..)"]
                    ,   T.concat ["import ",name,".Static.Wrappers exposing(..)"]
                    ,   T.concat ["import ",name,".Static.FromSuperPlace exposing (FromSuperPlace)"]
                    ,   T.concat ["import ",name,".Update exposing(..)"]
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
                    ,   "outgoingToIncoming : Transition -> Maybe IncomingMessage"
                    ,   "outgoingToIncoming trans ="
                    ,   "    case trans of"
                    ,   T.unlines $ mapMaybe o2i internalClientTransitions
                    ,   if length (mapMaybe o2i transitions) < length transitions then
                            "        _ -> Nothing"
                        else ""
                    ]
            
                placeMap :: M.Map T.Text HybridPlace
                placeMap = M.fromList $ map (\(pl@(HybridPlace n _ _ _ _ _)) -> (n,pl)) places

                getPlace :: T.Text -> HybridPlace
                getPlace name = M.findWithDefault (HybridPlace "" [] [] [] Nothing (Nothing,Nothing)) name placeMap

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
                    ,   "encodeTransition : Transition -> Maybe String"
                    ,   "encodeTransition trans ="
                    ,   "    case trans of"] ++
                    (if length internalClientTransitions > 0 then ["        Internal _ -> Nothing"] else [""]) ++
                    [   "        External ext -> Just <| encodeOutgoingTransition ext"
                    ,   generateEncoder Elm outgoingTransitions
                    ,   "--extra types encoders"
                    ,   T.unlines $ map (generateEncoder Elm) $ M.elems extraTypes
                    ]

                wOutgoingClientTransitions = map (\(n,t) -> ("T"++n,t)) $ map trans2constr (outgoingClientTransitions True)
                outgoingTransitions = ElmCustom "OutgoingTransition" wOutgoingClientTransitions
                incomingClientTransitions localOnly = 
                    concat $ mapMaybe (\tr ->
                        case tr of 
                            NetTransition tt (name,ets) clientTransitions _ -> 
                                Just $ mapMaybe (\(from,mTo) -> fmap snd mTo) clientTransitions 
                            ClientTransition constr _ _ ->
                                if localOnly then Nothing else Just [constr]
                            CmdTransition constr _ _ ->
                                Just [constr]) transitions
                incomingTransitions localOnly = 
                    ElmCustom "IncomingMessage" $ map (\(n,et) -> ("M"++n,et)) (incomingClientTransitions localOnly)
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
                        ,   "x = 0"
                       -- ,   T.unlines $ map (createWrap extraTypes (length places > 1) Elm "IncomingMessage" "M") incomingCM
                       ,   T.unlines $ map (createUnwrap Elm "OutgoingTransition" "T") $ map trans2constr (outgoingClientTransitions True)
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

                --static / rewriteable code
                createDirectoryIfMissing True $ fp </> "client" </> "src" </> T.unpack name </> "Static"
                createDirectoryIfMissing True $ fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Types"
                mapM_ (\pl -> writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Types" </> T.unpack (getPlaceName pl) <.> "elm") $ perPlaceTypes pl) places
                createDirectoryIfMissing True $ fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Wrappers"
                mapM_ (\pl -> writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Wrappers" </> T.unpack (getPlaceName pl) <.> "elm") $ perPlaceWrappers pl) places
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Types" <.> "elm") types
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Init" <.> "elm") hiddenInit
                writeIfNotExists (fp </> "client" </> "src" </> T.unpack name </> "Update" <.> "elm") update
                createDirectoryIfMissing True $ fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Helpers"
                mapM_ (\(HybridPlace pName _ _ edts _ _)  -> writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Helpers" </> T.unpack pName <.> "elm") $ T.unlines $ {-disclaimer currentTime :-} [generateHelper Elm name (T.unpack pName,edts) False]) places
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
