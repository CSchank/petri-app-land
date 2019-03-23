{-# LANGUAGE OverloadedStrings #-}

module Generate.Net.Server where

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
import Data.List (sort)

trans2constr :: Transition -> Constructor
trans2constr trans = 
    case trans of
        Transition _ constr _ _ -> constr
        CmdTransition constr _ _ -> constr

transName from msgN = T.concat [T.pack msgN,"from",from]


generate :: M.Map String CustomT -> FilePath -> Net -> IO ()
generate extraTypes fp net =
    case net of 
        (Net name startingPlace places allTransitions plugins) ->
            let
                transitions = mapMaybe (\t -> case t of
                                            Transition origin (name,ets) transLst mCmd -> 
                                                Just $ Transition origin (name,ets) (sort transLst) mCmd
                                            trans@(CmdTransition {}) ->
                                                Just trans
                                            _ -> Nothing
                                            ) allTransitions
                inits = T.unlines 
                    [
                    T.concat ["module ", name, ".Init where"]
                    ,T.concat["import ",name,".Static.Types"]
                    , "import Static.Cmd (Cmd)"
                    , "import Static.Cmd as Cmd"
                    , ""
                    , "-- the initial states of each place in this net"
                    , T.unlines $ map (generateNetInit extraTypes) places -- the initial places
                    ]
                placeStates = map (\(Place name placeState _ _ _ _) -> (T.unpack name,placeState)) places
                placeStateMap = M.fromList $ map (\(a,b) -> (a,(a,b))) placeStates
                placePlayerStates = map (\(Place name _ playerPlaceState _ _ _) -> (T.unpack $ T.concat[name,"Player"],playerPlaceState)) places
                placePlayerMap = M.fromList $ map (\(a,b) -> (a,(a,b))) placePlayerStates
                placeNames = map (\(Place name _ _ _ _ _) -> name) places
                -- the functions that the user changes
                generateNetInit :: M.Map String CustomT -> Place -> T.Text
                generateNetInit extraTypes (Place name serverPlaceState playerPlaceState _ mSubnet (mCmd,_)) = 
                    let
                        fnName = T.concat ["init",name]
                        typ = case mCmd of
                                Just cmdN -> T.concat [fnName, " :: (", capitalize name,", Cmd ",cmdN,")"]
                                Nothing       -> T.concat [fnName, " :: ", capitalize name]
                        decl = case mCmd of
                                Just _  -> T.concat [fnName, " = (", constr2Def extraTypes (T.unpack name,serverPlaceState),", Cmd.none)"]
                                Nothing -> T.concat [fnName, " = ", constr2Def extraTypes (T.unpack name,serverPlaceState)]
                    in
                        T.unlines 
                        [
                            typ
                        ,   decl
                        ]
                types = 
                    let
                        transitionImports = concatMap (concatMap (findImports Haskell) . snd) transConstrs
                        placeImports = concatMap (concatMap (findImports Haskell) . snd) placeStates
                        playerPlaceImports = concatMap (concatMap (findImports Haskell) . snd) placePlayerStates
                        imports :: [T.Text]
                        imports = fnub $ transitionImports ++ placeImports ++ playerPlaceImports
                    in
                    T.unlines 
                    [
                      "{-# LANGUAGE DeriveDataTypeable #-}"
                    , T.concat ["module ", name, ".Static.Types where"]
                    , "import Data.Typeable (Typeable)"
                    , "import Data.Data (Data)"
                    , "import Data.SafeCopy (SafeCopy)"
                    , T.unlines $ imports
                    , ""
                    , generateNetTypes name places
                    ]
                fromSuperPlace = 
                    T.unlines
                    [
                        T.concat["module ",name,".Static.FromSuperPlace where"]
                    ,   "import Static.ServerTypes"
                    ,   "type FromSuperPlace = TopLevelData" --FIXME: change this depending on where the net resides
                    ]
                clientMsgs :: [(String,Constructor)]
                clientMsgs = concat $ map (\tr -> case tr of
                                                    Transition _ (n,_) lstTrans _ ->
                                                                        mapMaybe (\(from,mTo) -> case mTo of 
                                                                                Just (to,msg) -> Just (n,msg)
                                                                                Nothing -> Nothing)
                                                                                    lstTrans
                                                    CmdTransition {} -> []
                                                                                    ) transitions
                outgoingCM = map (\(_,(msgN,edts)) -> (msgN,edts)) clientMsgs
                clientMsg = CustomT "ClientMessage" $ map (\(n,t) -> ("M"++n,t)) outgoingCM
                transitionType :: Transition -> [CustomT]
                transitionType (Transition transType (msgN,msg) connections mCmd) =
                    let                        
                        placeInputs :: [T.Text]
                        placeInputs = 
                            fnub $ map (\(from,_) -> from) connections
                        placeOutputs :: [T.Text]
                        placeOutputs = 
                            fnub $ mapMaybe (\(_,mTo) -> fmap fst mTo) connections
                        constructors :: (T.Text, [Maybe (T.Text, Constructor)]) -> [Constructor]
                        constructors (from,toLst) =
                            map (\mTo -> 
                                case mTo of 
                                    Just (to,(msgName,_))   -> constructor (T.unpack $ T.concat[T.pack msgN,"_",from,"to",to]) [edt (TypeT $ T.unpack $ T.concat [to,"Player"]) "" "", edt (TypeT msgName) "" ""]
                                    _                       -> constructor (T.unpack $ T.concat[T.pack msgN,"_Stay_",from]) [edt (TypeT $ T.unpack $ T.concat [from,"Player"]) "" ""]
                                        ) toLst
                    in
                        map (\(from,toLst) -> CustomT (T.unpack $ transName from msgN) $ constructors (from,toLst)) (grouped connections)
                transitionType _ = []
                transitionTxt :: Transition -> T.Text
                transitionTxt trans =
                    T.unlines $ map (generateType Haskell False [DOrd,DEq,DShow]) $ transitionType trans

                transConstrs :: [Constructor]
                transConstrs = map trans2constr transitions

                transType :: CustomT
                transType = ec "Transition" transConstrs

                generateNetTypes :: T.Text -> [Place] -> T.Text
                generateNetTypes netName places = 
                    let
                        placeModel = 
                            generateType Haskell False [DOrd,DEq,DShow] $ 
                                CustomT (T.unpack netName) $ map (\(Place n m _ _ _ _) -> (T.unpack n++"Player",[edt (TypeT $ T.unpack n) "" ""])) places
                        placeTypes = T.unlines $ map generatePlaceType places
                        generatePlaceType :: Place -> T.Text
                        generatePlaceType (Place name serverPlaceState playerPlaceState _ _ _) =
                            T.unlines
                                [
                                    generateType Haskell True [DOrd,DEq,DShow,DTypeable] $ CustomT (T.unpack name) [(T.unpack name, serverPlaceState)],""
                                ,   generateType Haskell True [DOrd,DEq,DShow,DTypeable] $ CustomT (T.unpack name++"Player") [(T.unpack name++"Player",playerPlaceState)],""
                                ]
                        playerUnionType = 
                            CustomT "Player" $ map (\(n,t) -> ("P"++n,t)) placePlayerStates
                        clientMsgType :: T.Text
                        clientMsgType =
                            let
                                
                            in
                                T.unlines $ map (\(_,msg@(msgN,edts)) -> generateType Haskell True [DOrd,DEq,DShow] $ CustomT msgN [msg]) clientMsgs
                                ++ [generateType Haskell True [DOrd,DEq,DShow] clientMsg]
                    in
                        T.unlines 
                            [
                                "-- place states and place player states"
                            ,   placeTypes
                            ,   "-- outgoing client message types"
                            ,   clientMsgType
                            ,   "-- individual transition types"
                            ,   T.unlines $ map transitionTxt transitions
                            ,   "-- main transition types"
                            ,   generateType Haskell True [DOrd,DEq,DShow] $ ec "Transition" $ map (\(n,t) -> ("T"++n,t)) transConstrs
                            ,   T.unlines $ map (\(n,et) -> generateType Haskell True [DOrd,DEq,DShow] $ ec n [(n,et)]) transConstrs
                            ,   "-- player state union type"
                            ,   generateType Haskell False [DOrd,DEq,DShow] playerUnionType
                            ,   "-- extra server types"
                            ,   T.unlines $ map (generateType Haskell True [DOrd,DEq,DShow] . snd) $ M.toList extraTypes
                            ]

                --singularTransFns :: [T.Text]
                singularTransFns msgN connections = map (\(from,lst) -> 
                                        let 
                                            output = transName from msgN
                                        in
                                            T.concat ["(ClientID, ",from,"Player) -> ",output]) (grouped connections)

                update :: T.Text
                update =
                    let
                        disconnectFn place = 
                            let
                                disconnectName = T.concat["clientDisconnectFrom",place]
                            in
                            T.unlines
                            [
                                    T.concat[disconnectName," :: FromSuperPlace -> ClientID -> ",place," -> ",place,"Player -> ",place]
                                ,   T.concat[disconnectName," fsp clientID ",uncapitalize place," ",uncapitalize place,"Player ="]
                                ,   T.concat["    error \"Please fill out the ",disconnectName," function for the ",name," net.\""]
                            ]
                    in T.unlines
                    [
                        T.concat ["module ",name,".Update where"]
                    ,   T.concat ["import ",name,".Static.Types"]
                    ,   T.concat ["import ",name,".Static.FromSuperPlace"]
                    ,   T.unlines $ map (\placeName -> T.concat["import ",name,".Static.Helpers.",placeName," as ",placeName]) placeNames
                    ,   T.unlines $ map (\placeName -> T.concat["import ",name,".Static.Helpers.",placeName,"Player as ",placeName,"Player"]) placeNames
                    ,   "import Static.List"
                    ,   "import Utils.Utils"
                    ,   "import Static.ServerTypes"
                    ,   "import Static.Cmd (Cmd(..))"
                    ,   "import qualified Static.Cmd as Cmd"
                    ,   ""
                    ,   "-- function called when new client connects (do not delete)"
                    ,   T.concat["clientConnect :: FromSuperPlace -> ClientID -> ",startingPlace," -> (", startingPlace,", ",startingPlace,"Player)"]
                    ,   T.concat["clientConnect fsp clientID ",uncapitalize startingPlace," ="]
                    ,   T.concat["    error \"Please fill out clientConnect function for the ",name," net.\""]
                    ,   ""
                    ,   "-- functions called when a client disconnects (do not delete)"
                    ,   T.unlines $ map disconnectFn $ M.keys placeMap
                    ,   "-- functions for each transition"
                    ,   T.unlines $ map generateTrans transitions
                    ]
                {-fromsTos :: (HybridTransition, Transition) -> [(T.Text,T.Text)]
                fromsTos (_, Transition (transName,_) connections mCmd) =
                    fnub $ map (\(from,(to,_)) -> (from,to)) connections-}
                fromsTos :: Transition -> ([T.Text],[T.Text])
                fromsTos (Transition _ (transName,_) connections mCmd) =
                    (fnub $ map (\(from,_) -> from) connections,fnub $ mapMaybe (\(from,mTo) -> if isJust mTo then fmap fst mTo else Just from) connections)
                fromsTos (CmdTransition _ place _) =
                    ([place],[place])
                fromsTos _ = ([],[])
                hiddenUpdate :: T.Text
                hiddenUpdate = 
                    let
                        {-playerFolder =
                                map (\(from,lst) -> 
                                    let 
                                    in
                                        T.unlines 
                                            [
                                                ""
                                            ]
                                        
                                        ) (grouped connections)-}
                        processTransPlayer tr@(Transition _ (transName,_) connections mCmd) = 
                            let
                                tfns = map (\t -> T.concat["(",t,")"]) $ singularTransFns transName (grouped connections)
                                froms = map (\t -> T.concat["from",t]) $ fst $ fromsTos tr
                            in
                            T.unlines
                            [
                                T.concat["process",T.pack transName,"Player :: ",T.intercalate " -> " $ tfns," -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))"]
                            ,   T.concat["process",T.pack transName,"Player ",T.intercalate " " froms," (cId,player) = case player of"]
                            ,   T.unlines $ map (\fromPlace -> 
                                        let
                                            (placeName,placeType) = getPlayerState $ getPlace $ fromPlace
                                        in
                                            T.concat ["    ",generatePattern ("P"++placeName++"Player", placeType)," -> let (np, mCm) = (unwrap",T.pack transName,"from",fromPlace," $ from",fromPlace," (cId,",generatePattern (placeName++"Player", placeType),")) in ((cId, np), (cId, mCm))"]
                                            ) $ fst $ fromsTos tr
                            ]
                        processTransPlayer _ = ""
                        splitPlayers tr@(Transition _ (transName,_) connections mCmd) = 
                            let
                                tfns = map (\t -> T.concat["(",t,")"]) $ singularTransFns transName (grouped connections)
                                froms = map (\t -> T.concat["from",t]) $ fst $ fromsTos tr
                                foldTuple = T.concat["(",T.intercalate "," $ map (\t -> T.concat [t,"lst"]) froms,")"]
                            in
                            T.unlines
                            [
                                T.concat["split",T.pack transName,"Players :: [(ClientID,Player)] -> (",T.intercalate "," $ map (\p -> T.concat["[(ClientID,",p,"Player)]"]) $ fst $ fromsTos tr,")"]
                            ,   T.concat["split",T.pack transName,"Players players = foldl (\\t@",foldTuple," pl -> case pl of"]
                            ,   T.unlines $ map (\(fromPlace,n) -> 
                                        let
                                            (placeName,placeType) = getPlayerState $ getPlace $ fromPlace
                                        in
                                            T.concat ["    (cId,p@(",generatePattern ("P"++placeName++"Player", placeType),")) -> (" ,T.intercalate "," $ map (\(f,m) -> if n == m then T.concat["(cId,",generatePattern (placeName++"Player", placeType),"):",f,"lst"] else T.concat[f,"lst"]) $ zip froms [0..],")"]
                                            ) $ zip (fst $ fromsTos tr) [0..]
                            ,   T.concat ["    _ -> t) (",T.intercalate "," $ replicate (length froms) "[]",") players"]
                            ]
                        splitPlayers _ = ""
                        transCase tr@(Transition transType constr@(transName,transArgs) _ mCmd) = 
                            let
                                froms = fst $ fromsTos tr
                                tos = snd $ fromsTos tr
                                allStates = fnub $ froms ++ tos
                                fromVars = map (\t -> T.concat["from",t]) froms
                                transTxt = T.pack transName
                                clientIdTxt = 
                                    case transType of
                                        OriginEitherPossible     -> "mClientID "
                                        OriginClientOnly -> "(fromJust mClientID) "
                                        OriginServerOnly -> ""
                                cmd =
                                    case mCmd of 
                                        Just c -> T.concat ["Just $ Cmd.map unwrap",c," cmd"]
                                        Nothing -> "Nothing"
                                transContr = trans2constr tr
                            in
                            T.unlines $ map (\t -> T.concat["                ",t]) $
                            [
                                T.concat [generatePattern ("T"++transName,transArgs),  "->"]
                            ,   "    let"
                            ,   T.concat["        (",T.intercalate "," $ map (\t -> T.concat[uncapitalize t,"PlayerLst"]) froms,") = split",transTxt,"Players (IM'.toList players)"]
                            ,   case mCmd of
                                    Just cmd -> 
                                        T.concat ["        (",T.intercalate "," $ map uncapitalize allStates,",",T.intercalate "," fromVars,",cmd) = update",transTxt," tld ",clientIdTxt,"(",generatePattern transContr,")",T.replicate (length allStates) "(fromJust $ TM.lookup places) ", T.intercalate " " $ map (\t -> T.concat ["(map snd ",uncapitalize t, "PlayerLst)"]) froms]
                                    Nothing ->
                                        T.concat ["        (",T.intercalate "," $ map uncapitalize allStates,",",T.intercalate "," fromVars,") = update",transTxt," tld ",clientIdTxt,"(",generatePattern transContr,") ",T.replicate (length allStates) "((safeFromJust \"place lookup\") $ TM.lookup places) ", T.intercalate " " $ map (\t -> T.concat ["(map snd ",uncapitalize t, "PlayerLst)"]) froms]
                            ,   T.concat["        newPlaces = ",T.intercalate " $ " $ map (\state -> T.concat["TM.insert ",uncapitalize state]) allStates, " places"]
                            ,   T.concat["        (newPlayers, clientMessages) = unzip $ map (process",transTxt,"Player ",T.intercalate " " fromVars,") (",T.intercalate "++" $ map (\from -> T.concat["mapSnd unwrap",from,"Player ",uncapitalize from,"PlayerLst"]) froms,")"]
                            ,   "    in"
                            ,   T.concat["        (newPlaces, newPlayers, clientMessages, ", cmd,")" ]
                            ]
                        transCase tr@(CmdTransition constr@(transName,transArgs) place cmd) = 
                            let
                                
                            in
                            T.unlines $ map (\t -> T.concat["                ",t]) $
                            [
                                T.concat [generatePattern ("T"++transName,transArgs),  "->"]
                            ,   T.concat ["     (places,[],[],Just $ Cmd.map unwrap",cmd," $ cmd_",T.pack transName," (fromJust mClientID) ",generatePattern constr,")"]
                            ]
                        transCase _ = ""
                        disconnectCase fromPlace = 
                            let
                                (placeName,placeType) = getPlayerState $ getPlace $ fromPlace
                            in
                            T.concat ["            ",generatePattern ("P"++placeName++"Player", placeType)," -> (flip TM.insert) places $ clientDisconnectFrom",fromPlace," fsp clientID (safeFromJust \"clientDisconnectFrom\" $ TM.lookup places) (",generatePattern (placeName++"Player", placeType),")"]
                    in T.unlines
                    [
                        T.concat ["module ",name,".Static.Update where"]
                    ,   T.concat ["import ",name,".Static.Types"]
                    ,   T.concat ["import ",name,".Static.Wrappers"]
                    ,   T.concat ["import ",name,".Static.FromSuperPlace (FromSuperPlace(..))"]
                    ,   T.concat ["import ",name,".Update as Update"]
                    ,   "import qualified Static.Cmd as Cmd"
                    ,   "import qualified Data.TMap as TM"
                    ,   "import Static.ServerTypes"
                    ,   "import qualified Data.IntMap.Strict as IM'"
                    ,   "import Data.Maybe (fromJust, isJust, mapMaybe)"
                    ,   "import Utils.Utils"
                    ,   "import Static.Cmd (Cmd)"
                    ,   "import Plugins.Users (processLogout, Users)"
                    ,   "import Static.Task (evalTask)"
                    ,   ""
                    ,   "-- player processing functions"
                    ,   T.unlines $ map processTransPlayer transitions
                    ,   "-- player splitting functions"
                    ,   T.unlines $ map splitPlayers transitions
                    ,   "-- process player connect"
                    ,   "clientConnect :: FromSuperPlace -> ClientID -> NetState Player -> NetState Player"
                    ,   "clientConnect fsp clientID state ="
                    ,   "    let"
                    ,   T.concat["        (",uncapitalize startingPlace,",",uncapitalize startingPlace,"Player) = Update.clientConnect fsp clientID (safeFromJust \"client connect Net\" $ TM.lookup $ placeStates state)"]
                    ,   "    in"
                    ,   T.concat["        state { placeStates = TM.insert ",uncapitalize startingPlace," $ placeStates state, playerStates = IM'.insert clientID (unwrap",startingPlace,"Player ",uncapitalize startingPlace,"Player) (playerStates state) }"]
                    ,   ""
                    ,   "-- process player disconnects"
                    ,   "disconnect :: FromSuperPlace -> ClientID -> NetState Player -> IO (NetState Player)"
                    ,   "disconnect fsp clientID state ="
                    ,   "    let"
                    ,   "        player = safeFromJust \"disconnect\" $ IM'.lookup clientID $ players"
                    ,   "        places = placeStates state"
                    ,   "        players = playerStates state"
                    ,   "        newPlaces = case player of"
                    ,   T.unlines $ map disconnectCase placeNames
                    ,   "        newPlayers = IM'.delete clientID players"
                    ,   "        task = processLogout clientID"
                    ,   "    in do"
                    ,   "        case (TM.lookup $ pluginStates state :: Maybe Plugins.Users.Users) of"
                    ,   "            Just _ -> do"
                    ,   "                evalTask (pluginStates state) task -- back door into the users plugin..."
                    ,   "                return ()"
                    ,   "            Nothing -> return ()"
                    ,   "        return $ state { playerStates = newPlayers, placeStates = newPlaces }"
                    ,   ""
                    ,   T.concat ["update :: TopLevelData -> Maybe ClientID -> Transition -> NetState Player -> (NetState Player,[(ClientID,ClientMessage)],Maybe (Cmd.Cmd Transition))"]
                    ,   T.concat ["update tld mClientID trans state ="]
                    ,   "    let"
                    ,   "        places = placeStates state"
                    ,   "        players = playerStates state"
                    ,   "        (newPlaces, newPlayers, clientMessages, cmd) = "
                    ,   "            case trans of"
                    ,   T.unlines $ map transCase transitions
                    ,   "    in"
                    ,   "        (state"
                    ,   "           {"
                    ,   "                placeStates = newPlaces"
                    ,   "           ,    playerStates = insertList newPlayers $ players"
                    ,   "           }"
                    ,   "        , mapMaybe (\\(a,b) -> if isJust b then Just (a,fromJust b) else Nothing) clientMessages"
                    ,   "        , cmd)"
                    ]
            
                placeMap :: M.Map T.Text Place
                placeMap = M.fromList $ map (\(pl@(Place n _ _ _ _ _)) -> (n,pl)) places

                getPlace :: T.Text -> Place
                getPlace name = M.findWithDefault (Place "" [] [] [] Nothing (Nothing,Nothing)) name placeMap

                generateTrans :: Transition -> T.Text
                generateTrans (Transition transType (msgN,msg) connections mCmd) =
                    let
                        pattern = generatePattern (msgN,msg)

                        placeInputs :: [T.Text]
                        placeInputs = 
                            fnub $ map (\(from,_) -> from) connections
                        placeOutputs :: [T.Text]
                        placeOutputs = 
                            fnub $ mapMaybe (\(_,mTo) -> fmap fst mTo) connections
                        clientIdType = case transType of 
                            OriginEitherPossible -> "    Maybe ClientID ->\n"
                            OriginClientOnly -> "    ClientID ->\n"
                            OriginServerOnly -> ""
                        
                        clientId = case transType of 
                            OriginEitherPossible -> "mClientId"
                            OriginClientOnly -> "clientId"
                            OriginServerOnly -> ""

                        fnName = T.concat["update",T.pack msgN]
                        outputs = fnub $ placeInputs ++ placeOutputs ++ singularTransFns msgN connections ++ map (\t -> T.concat["Cmd ",t]) cmds

                        oneOfs = T.concat $ "OneOf" : map (\txt -> T.concat[txt,"Player"]) placeOutputs
                        typ = T.concat  [ fnName," :: FromSuperPlace -> \n"
                                        , clientIdType
                                        , "    ", T.pack msgN," ->\n    "
                                        , T.intercalate " -> \n    " (fnub $ placeInputs ++ placeOutputs ++ map (\txt -> T.concat["List ",txt,"Player"]) placeInputs), " -> \n"
                                        , T.concat ["    ( ",T.intercalate ",\n      " outputs,"\n    )"]
                                        ] 
                        
                        decl = T.concat [ fnName," fsp "
                                        , clientId," "
                                        , pattern
                                        , T.intercalate " " $ fnub $ map uncapitalize $ (placeInputs ++ placeOutputs ++ map (\txt -> T.concat["lst",txt]) placeInputs), " ="
                                        ] 
                        singularStubs = 
                                        map (\(from,lst) -> 
                                                let 
                                                    oneOfs = map (\mTo -> case mTo of 
                                                        Just (to,(msg,_)) -> T.concat["(P",to,", ",T.pack msg,")"]
                                                        Nothing  -> T.concat[from,"Player"]
                                                        ) lst
                                                    output = transName from msgN
                                                    name = T.concat ["        from",from]
                                                    typ = T.concat [name, " :: (ClientID, ",from,"Player) -> ",output]
                                                in
                                                    T.unlines 
                                                    [
                                                        typ
                                                    ,   T.concat [name," (pId, p",uncapitalize from,") = error \"Please fill in function stub.\""]
                                                    ]) (grouped connections)
                        cmds = 
                            case mCmd of 
                                Just msgN -> [msgN]
                                Nothing -> []

                        in T.unlines 
                        [
                            typ
                        ,   decl
                        ,   "    let"
                        ,   T.unlines singularStubs
                        ,   "    in"
                        ,   T.concat["        ",T.concat ["(",T.intercalate ", " $ fnub $ map uncapitalize (placeInputs ++ placeOutputs),", "
                                                         ,T.intercalate ", " $ map (\txt -> T.concat ["from",txt]) placeInputs
                                                         ,T.replicate (length cmds) ", Cmd.none"
                                                         ,")"]
                                                         ]
                                    ]
                generateTrans (CmdTransition msg@(msgN,msgT) place cmd) =
                    let
                        fnName = T.concat ["cmd_",T.pack msgN]
                        typ = T.concat [fnName, " :: ClientID -> ", T.pack msgN," -> Cmd ",cmd]
                        decl = T.concat [fnName, " clientId ", generatePattern msg, " ="]
                    in
                        T.unlines 
                        [
                            typ
                        ,   decl
                        ,   T.concat["    error \"Please fill out function",fnName," in the Update module for net", name,"\""]
                        ]
                generateTrans _ = ""

                hiddenInit = T.unlines 
                    [
                        T.concat ["module ",name,".Static.Init where"]
                    ,   T.concat ["import ",name,".Static.Types (Player)"]
                    ,   T.concat ["import ",name,".Init as Init"]
                    ,   T.concat ["import ",name,".Update as Update"]
                    ,   T.concat ["import ",name,".Static.Wrappers"]
                    ,   T.concat ["import ",name,".Static.Plugins (initPlugins,teardownPlugins)"]
                    ,   "import Static.ServerTypes"
                    ,   "import qualified Data.IntMap as IM'"
                    ,   "import qualified Data.TMap as TM\n"
                    ,   "init :: IO (NetState Player)"
                    ,   "init = do"
                    ,   "    ip <- initPlugins"
                    ,   "    return $ NetState"
                    ,   "        {"
                    ,   "          playerStates = IM'.empty"
                    ,   T.concat["        , placeStates = ",T.concat $ map (\(Place name _ _ _ _ (mCmd,_)) -> T.concat["TM.insert",if isJust mCmd then T.concat[" (fst init",name] else T.concat[" init",name]," $ "]) places,"TM.empty"]
                    ,   T.concat["        , pluginStates = ip"]
                    ,   "        }"
                    ,   "teardown :: NetState Player -> IO ()"
                    ,   "teardown ns = do"
                    ,   "    teardownPlugins (pluginStates ns)"
                    ]
                encoder = T.unlines 
                    [
                        "{-# LANGUAGE OverloadedStrings #-}"
                    ,   T.concat ["module ",name,".Static.Encode where"]
                    ,   T.concat ["import ",name,".Static.Types\n"]
                    ,   "import Utils.Utils"
                    ,   "import qualified Data.Text as T"
                    ,   "import Static.Types"
                    ,   "import Data.Map.Strict as Dict"
                    ,   "import qualified Static.Result as Result"
                    ,   generateEncoder Haskell clientMsg
                    ,   "-- extra type encoders"
                    ,   T.unlines $ map (generateEncoder Haskell) $ M.elems extraTypes
                    ]
                incomingClientTransitions = 
                    mapMaybe 
                        (\tr -> case tr of
                            Transition tt (name,ets) _ _ ->
                                if tt == OriginEitherPossible || tt == OriginClientOnly then 
                                    Just ("T"++name,ets) 
                                else Nothing
                            CmdTransition (name,ets) _ _ ->
                                Just ("T"++name,ets)
                            ClientTransition {} -> Nothing
                            ) transitions
                clientTransitions = CustomT "Transition" incomingClientTransitions
                decoder = T.unlines 
                    [
                        "{-# LANGUAGE OverloadedStrings #-}"
                    ,   T.concat ["module ",name,".Static.Decode where"]
                    ,   T.concat ["import ",name,".Static.Types\n"]
                    ,   "import Utils.Utils"
                    ,   "import Static.Result (Result(..))"
                    ,   "import qualified Data.Text as T"
                    ,   "import qualified Static.Result as Result"
                    ,   generateDecoder Haskell clientTransitions
                    ,   "-- extra type decoders"
                    ,   T.unlines $ map (generateDecoder Haskell) $ M.elems extraTypes
                    ]
                wrappers = 
                    T.unlines
                        [
                            T.concat ["module ",name,".Static.Wrappers where"]
                        ,   "import Static.Dict"
                        ,   "import Data.Map.Strict as Dict"
                        ,   T.concat ["import ",name,".Static.Types\n"]
                        ,   T.unlines $ map (createUnwrap Haskell "ClientMessage" "M") outgoingCM
                        --,   T.unlines $ map (createWrap extraTypes (length places > 1) Haskell "ClientMessage" "M") outgoingCM
                        ,   T.unlines $ map (createUnwrap Haskell "Player" "P") placePlayerStates
                        --,   T.unlines $ map (createWrap extraTypes (length places > 1) Haskell "Player" "P") placePlayerStates
                        ,   T.unlines $ map (createTransitionUnwrap (length places > 1) Haskell) transitions
                        ,   T.unlines $ map (createUnwrap Haskell "Transition" "T") transConstrs
                        --,   T.unlines $ map (createWrap extraTypes (length transitions > 1) Haskell "Transition" "T") transConstrs
                        ]        
            in do
                createDirectoryIfMissing True $ fp </> "server" </> "src" </> T.unpack name
                createDirectoryIfMissing True $ fp </> "server" </> "src" </> T.unpack name </> "Static"
                writeIfNotExists (fp </> "server" </> "src" </> T.unpack name </> "Init" <.> "hs") inits 
                writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Types" <.> "hs") types
                writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Init" <.> "hs") hiddenInit

                createDirectoryIfMissing True $ fp </> "server" </> "src" </> T.unpack name </> "Templates"
                writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Templates" </> "Update" <.> "txt") update
                writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Templates" </> "init" <.> "txt") inits
                writeIfNotExists (fp </> "server" </> "src" </> T.unpack name </> "Update" <.> "hs") update
                createDirectoryIfMissing True $ fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Helpers"
                mapM_ (\(Place pName edts _ _ _ _)  -> writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Helpers" </> T.unpack pName <.> "hs") $ T.unlines $ {-disclaimer currentTime :-} [generateHelper Haskell name (T.unpack pName,edts) False]) places
                mapM_ (\(Place pName _ pEdts _ _ _) -> writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Helpers" </> T.unpack pName ++ "Player" <.> "hs") $ T.unlines $ {-disclaimer currentTime :-} [generateHelper Haskell name (T.unpack pName ++ "Player",pEdts) False]) places
                writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Encode" <.> "hs") encoder
                writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Decode" <.> "hs") decoder
                writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Wrappers" <.> "hs") wrappers
                writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Update" <.> "hs") hiddenUpdate
                writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "FromSuperPlace" <.> "hs") fromSuperPlace
                generatePlugins (fp </> "server" </> "src") extraTypes net plugins
        _ -> return ()