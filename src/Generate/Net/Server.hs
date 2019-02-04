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

trans2constr :: NetTransition -> Constructor
trans2constr trans = 
    case trans of
        NetTransition constr _ _ -> constr

transName from msgN = T.concat [T.pack msgN,"from",from]


generate :: M.Map String ElmCustom -> FilePath -> Net -> IO ()
generate extraTypes fp net =
    case net of 
        (HybridNet name startingPlace places transitions plugins) ->
            let
                inits = T.unlines 
                    [
                    T.concat ["module ", name, ".Init where"]
                    ,T.concat["import ",name,".Static.Types"]
                    , ""
                    , "-- the initial states of each place in this net"
                    , T.unlines $ map (generateNetInit extraTypes) places -- the initial places
                    ]
                placePlayerStates = map (\(HybridPlace name _ playerPlaceState _ _ _ _) -> (T.unpack $ T.concat[name,"Player"],playerPlaceState)) places
                placeNames = map (\(HybridPlace name _ _ _ _ _ _) -> name) places
                -- the functions that the user changes
                generateNetInit :: M.Map String ElmCustom -> HybridPlace -> T.Text
                generateNetInit extraTypes (HybridPlace name serverPlaceState playerPlaceState _ mSubnet (mCmd,_) _) = 
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
                types = T.unlines 
                    [
                    T.concat ["module ", name, ".Static.Types where"]
                    , "import Data.Typeable (Typeable)"
                    , "import Static.List"
                    , ""
                    , "-- the initial state of all places in this net"
                    , generateNetTypes name places -- the initial places
                    , "-- the FromSuperPlace type"
                    ]
                fromSuperPlace = 
                    T.unlines
                    [
                        T.concat["module ",name,".Static.FromSuperPlace where"]
                    ,   "import Static.ServerTypes"
                    ,   "type FromSuperPlace = TopLevelData" --FIXME: change this depending on where the net resides
                    ]
                clientMsgs :: [(String,Constructor)]
                clientMsgs = concat $ map (\(_,NetTransition (n,_) lstTrans _) -> 
                                            mapMaybe (\(from,mTo) -> case mTo of 
                                                                        Just (to,msg) -> Just (n,msg)
                                                                        Nothing -> Nothing)
                                                                            lstTrans) transitions
                outgoingCM = map (\(_,(msgN,edts)) -> (msgN,edts)) clientMsgs
                clientMsg = ElmCustom "ClientMessage" $ map (\(n,t) -> ("M"++n,t)) outgoingCM
                transitionType :: (HybridTransition,NetTransition) -> [ElmCustom]
                transitionType (transType, NetTransition (msgN,msg) connections mCmd) =
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
                                    Just (to,(msgName,_))   -> constructor (T.unpack $ T.concat[T.pack msgN,"_",from,"to",to]) [edt (ElmType $ T.unpack $ T.concat [to,"Player"]) "" "", edt (ElmType msgName) "" ""]
                                    _                       -> constructor (T.unpack $ T.concat[T.pack msgN,"_Stay_",from]) [edt (ElmType $ T.unpack $ T.concat [from,"Player"]) "" ""]
                                        ) toLst
                    in
                        map (\(from,toLst) -> ElmCustom (T.unpack $ transName from msgN) $ constructors (from,toLst)) (grouped connections)
                transitionTxt :: (HybridTransition, NetTransition) -> T.Text
                transitionTxt trans =
                    T.unlines $ map (generateType Haskell False [DOrd,DEq,DShow]) $ transitionType trans

                transConstrs :: [Constructor]
                transConstrs = map (trans2constr . snd) transitions

                transType :: ElmCustom
                transType = ec "Transition" transConstrs

                generateNetTypes :: T.Text -> [HybridPlace] -> T.Text
                generateNetTypes netName places = 
                    let
                        placeModel = 
                            generateType Haskell False [DOrd,DEq,DShow] $ 
                                ElmCustom (T.unpack netName) $ map (\(HybridPlace n m _ _ _ _ _) -> (T.unpack n++"Player",[edt (ElmType $ T.unpack n) "" ""])) places
                        placeTypes = T.unlines $ map generatePlaceType places
                        generatePlaceType :: HybridPlace -> T.Text
                        generatePlaceType (HybridPlace name serverPlaceState playerPlaceState _ _ _ _) =
                            T.unlines
                                [
                                    generateType Haskell True [DOrd,DEq,DShow,DTypeable] $ ElmCustom (T.unpack name) [(T.unpack name, serverPlaceState)],""
                                ,   generateType Haskell True [DOrd,DEq,DShow,DTypeable] $ ElmCustom (T.unpack name++"Player") [(T.unpack name++"Player",playerPlaceState)],""
                                ]
                        playerUnionType = 
                            ElmCustom "Player" $ map (\(n,t) -> ("P"++n,t)) placePlayerStates
                        clientMsgType :: T.Text
                        clientMsgType =
                            let
                                
                            in
                                T.unlines $ map (\(_,msg@(msgN,edts)) -> generateType Haskell True [DOrd,DEq,DShow] $ ElmCustom msgN [msg]) clientMsgs
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
                    ,   "import Static.List"
                    ,   "import Utils.Utils"
                    ,   "import Static.ServerTypes"
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
                {-fromsTos :: (HybridTransition, NetTransition) -> [(T.Text,T.Text)]
                fromsTos (_, NetTransition (transName,_) connections mCmd) =
                    fnub $ map (\(from,(to,_)) -> (from,to)) connections-}
                fromsTos :: (HybridTransition, NetTransition) -> ([T.Text],[T.Text])
                fromsTos (_, NetTransition (transName,_) connections mCmd) =
                    (fnub $ map (\(from,_) -> from) connections,fnub $ mapMaybe (\(from,mTo) -> if isJust mTo then fmap fst mTo else Just from) connections)
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
                        processTransPlayer (tr@(_, NetTransition (transName,_) connections mCmd)) = 
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
                                            T.concat ["    ",generatePattern ("P"++placeName++"Player", placeType)," -> let (np, mCm) = (unwrap",T.pack transName,"from",fromPlace," $ from",fromPlace," (cId,wrap",fromPlace,"Player player)) in ((cId, np), (cId, mCm))"]
                                            ) $ fst $ fromsTos tr
                            ]
                        splitPlayers (tr@(_, NetTransition (transName,_) connections mCmd)) = 
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
                                            placeState = getPlayerState $ getPlace $ fromPlace
                                        in
                                            T.concat ["    (cId,p@(P",fromPlace,"Player {})) -> (" ,T.intercalate "," $ map (\(f,m) -> if n == m then T.concat["(cId,wrap", fromPlace,"Player p):",f,"lst"] else T.concat[f,"lst"]) $ zip froms [0..],")"]
                                            ) $ zip (fst $ fromsTos tr) [0..]
                            ,   T.concat ["    _ -> t) (",T.intercalate "," $ replicate (length froms) "[]",") players"]
                            ]
                        transCase tr@(transType, NetTransition constr@(transName,transArgs) _ mCmd) = 
                            let
                                froms = fst $ fromsTos tr
                                tos = snd $ fromsTos tr
                                allStates = fnub $ froms ++ tos
                                fromVars = map (\t -> T.concat["from",t]) froms
                                transTxt = T.pack transName
                                clientIdTxt = 
                                    case transType of
                                        HybridTransition     -> "mClientID "
                                        ClientOnlyTransition -> "(fromJust mClientID) "
                                        ServerOnlyTransition -> ""
                            in
                            T.unlines $ map (\t -> T.concat["                ",t]) $
                            [
                                T.concat [generatePattern ("T"++transName,transArgs),  "->"]
                            ,   "    let"
                            ,   T.concat["        (",T.intercalate "," $ map (\t -> T.concat[uncapitalize t,"PlayerLst"]) froms,") = split",transTxt,"Players (IM'.toList players)"]
                            ,   case mCmd of
                                    Just cmd -> 
                                        T.concat ["        (",T.intercalate "," $ map uncapitalize allStates,",",T.intercalate "," fromVars,",cmd) = update",transTxt," tld ",clientIdTxt,"(wrap",transTxt," trans)",T.replicate (length allStates) "(fromJust $ TM.lookup places) ", T.intercalate " " $ map (\t -> T.concat ["(map snd ",uncapitalize t, "PlayerLst)"]) froms]
                                    Nothing ->
                                        T.concat ["        (",T.intercalate "," $ map uncapitalize allStates,",",T.intercalate "," fromVars,") = update",transTxt," tld ",clientIdTxt,"(wrap",transTxt," trans) ",T.replicate (length allStates) "((safeFromJust \"place lookup\") $ TM.lookup places) ", T.intercalate " " $ map (\t -> T.concat ["(map snd ",uncapitalize t, "PlayerLst)"]) froms]
                            ,   T.concat["        newPlaces = ",T.intercalate " $ " $ map (\state -> T.concat["TM.insert ",uncapitalize state]) allStates, " places"]
                            ,   T.concat["        (newPlayers, clientMessages) = unzip $ map (process",transTxt,"Player ",T.intercalate " " fromVars,") (",T.intercalate "++" $ map (\from -> T.concat["mapSnd unwrap",from,"Player ",uncapitalize from,"PlayerLst"]) froms,")"]
                            ,   "    in"
                            ,   T.concat["        (newPlaces, newPlayers, clientMessages, ", if isJust mCmd then "Just cmd" else "Nothing",")" ]
                            ]
                        disconnectCase placeName = 
                            T.concat ["            P",placeName,"Player {} -> (flip TM.insert) places $ clientDisconnectFrom",placeName," fsp clientID (safeFromJust \"clientDisconnectFrom\" $ TM.lookup places) (wrap",placeName,"Player player)"]
                    in T.unlines
                    [
                        T.concat ["module ",name,".Static.Update where"]
                    ,   T.concat ["import ",name,".Static.Types"]
                    ,   T.concat ["import ",name,".Static.Wrappers"]
                    ,   T.concat ["import ",name,".Static.FromSuperPlace (FromSuperPlace(..))"]
                    ,   T.concat ["import ",name,".Update as Update"]
                    ,   "import qualified Data.TMap as TM"
                    ,   "import Static.ServerTypes"
                    ,   "import qualified Data.IntMap.Strict as IM'"
                    ,   "import Data.Maybe (fromJust, isJust, mapMaybe)"
                    ,   "import Utils.Utils"
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
                    ,   "disconnect :: FromSuperPlace -> ClientID -> NetState Player -> NetState Player"
                    ,   "disconnect fsp clientID state ="
                    ,   "    let"
                    ,   "        player = safeFromJust \"disconnect\" $ IM'.lookup clientID $ players"
                    ,   "        places = placeStates state"
                    ,   "        players = playerStates state"
                    ,   "        newPlaces = case player of"
                    ,   T.unlines $ map disconnectCase placeNames
                    ,   "        newPlayers = IM'.delete clientID players"
                    ,   "    in"
                    ,   "        state { playerStates = newPlayers, placeStates = newPlaces }"
                    ,   ""
                    ,   T.concat ["update :: TopLevelData -> Maybe ClientID -> Transition -> NetState Player -> (NetState Player,[(ClientID,ClientMessage)],Maybe (Cmd Transition))"]
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
            
                placeMap :: M.Map T.Text HybridPlace
                placeMap = M.fromList $ map (\(pl@(HybridPlace n _ _ _ _ _ _)) -> (n,pl)) places

                getPlace :: T.Text -> HybridPlace
                getPlace name = M.findWithDefault (HybridPlace "" [] [] [] Nothing (Nothing,Nothing) Nothing) name placeMap

                generateTrans :: (HybridTransition,NetTransition) -> T.Text
                generateTrans (transType, NetTransition (msgN,msg) connections mCmd) =
                    let
                        pattern = generatePattern (msgN,msg)

                        placeInputs :: [T.Text]
                        placeInputs = 
                            fnub $ map (\(from,_) -> from) connections
                        placeOutputs :: [T.Text]
                        placeOutputs = 
                            fnub $ mapMaybe (\(_,mTo) -> fmap fst mTo) connections
                        clientIdType = case transType of 
                            HybridTransition -> "    Maybe ClientID ->\n"
                            ClientOnlyTransition -> "    ClientID ->\n"
                            ServerOnlyTransition -> ""
                        
                        clientId = case transType of 
                            HybridTransition -> "mClientId"
                            ClientOnlyTransition -> "clientId"
                            ServerOnlyTransition -> ""

                        fnName = T.concat["update",T.pack msgN]
                        outputs = fnub $ placeInputs ++ placeOutputs ++ singularTransFns msgN connections ++ cmds

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
                                Just (msgN,msg) -> [T.pack msgN]
                                Nothing -> []

                    {-hiddenUpdate :: T.Text
                    hiddenUpdate = 
                        let
                            decl = T.concat ["update",]
                        in T.unlines
                            [
                                T.concat ["module ",name,".Static.Update where"]
                            ,   T.concat ["import ",name,".Update"]
                            ,   T.unlines $ map generateHiddenUpdate transitions
                            ]-}
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
                hiddenInit = T.unlines 
                    [
                        T.concat ["module ",name,".Static.Init where"]
                    ,   T.concat ["import ",name,".Static.Types (Player)"]
                    ,   T.concat ["import ",name,".Init as Init"]
                    ,   T.concat ["import ",name,".Update as Update"]
                    ,   T.concat ["import ",name,".Static.Wrappers"]
                    ,   T.concat ["import ",name,".Static.Plugins (initPlugins)"]
                    ,   "import Static.ServerTypes"
                    ,   "import qualified Data.IntMap as IM'"
                    ,   "import qualified Data.TMap as TM\n"
                    ,   "init :: IO (NetState Player)"
                    ,   "init = do"
                    ,   "    ip <- initPlugins"
                    ,   "    return $ NetState"
                    ,   "        {"
                    ,   "          playerStates = IM'.empty"
                    ,   T.concat["        , placeStates = ",T.concat $ map (\(HybridPlace name _ _ _ _ (mCmd,_) _) -> T.concat["TM.insert",if isJust mCmd then T.concat[" (fst init",name] else T.concat[" init",name]," $ "]) places,"TM.empty"]
                    ,   T.concat["        , pluginStates = ip"]
                    ,   "        }"
                    ]
                encoder = T.unlines 
                    [
                        "{-# LANGUAGE OverloadedStrings #-}"
                    ,   T.concat ["module ",name,".Static.Encode where"]
                    ,   T.concat ["import ",name,".Static.Types\n"]
                    ,   "import Utils.Utils"
                    ,   "import qualified Data.Text as T"
                    ,   "import Static.Types"
                    ,   generateEncoder Haskell clientMsg
                    ,   "-- extra type encoders"
                    ,   T.unlines $ map (generateEncoder Haskell) $ M.elems extraTypes
                    ]
                incomingClientTransitions = mapMaybe (\(tt,NetTransition (name,ets) _ _) -> if tt == HybridTransition || tt == ClientOnlyTransition then Just ("T"++name,ets) else Nothing) transitions
                clientTransitions = ElmCustom "Transition" incomingClientTransitions
                decoder = T.unlines 
                    [
                        "{-# LANGUAGE OverloadedStrings #-}"
                    ,   T.concat ["module ",name,".Static.Decode where"]
                    ,   T.concat ["import ",name,".Static.Types\n"]
                    ,   "import Utils.Utils"
                    ,   "import qualified Data.Text as T"
                    ,   generateDecoder Haskell clientTransitions
                    ,   "-- extra type decoders"
                    ,   T.unlines $ map (generateDecoder Haskell) $ M.elems extraTypes
                    ]
                wrappers = 
                    T.unlines
                        [
                            T.concat ["module ",name,".Static.Wrappers where"]
                        ,   T.concat ["import ",name,".Static.Types\n"]
                        ,   T.unlines $ map (createUnwrap Haskell "ClientMessage" "M") outgoingCM
                        ,   T.unlines $ map (createWrap extraTypes (length places > 1) Haskell "ClientMessage" "M") outgoingCM
                        ,   T.unlines $ map (createUnwrap Haskell "Player" "P") placePlayerStates
                        ,   T.unlines $ map (createWrap extraTypes (length places > 1) Haskell "Player" "P") placePlayerStates
                        ,   T.unlines $ map (createTransitionUnwrap (length places > 1) Haskell) transitions
                        ,   T.unlines $ map (createUnwrap Haskell "Transition" "T") transConstrs
                        ,   T.unlines $ map (createWrap extraTypes (length transitions > 1) Haskell "Transition" "T") transConstrs
                        ]        
            in do
                createDirectoryIfMissing True $ fp </> "server" </> "src" </> T.unpack name
                createDirectoryIfMissing True $ fp </> "server" </> "src" </> T.unpack name </> "Static"
                writeIfNotExists (fp </> "server" </> "src" </> T.unpack name </> "Init" <.> "hs") inits 
                writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Types" <.> "hs") types
                writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Init" <.> "hs") hiddenInit
                writeIfNotExists (fp </> "server" </> "src" </> T.unpack name </> "Update" <.> "hs") update
                createDirectoryIfMissing True $ fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Helpers"
                mapM_ (\(HybridPlace pName edts _ _ _ _ _)  -> writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Helpers" </> T.unpack pName <.> "hs") $ T.unlines $ {-disclaimer currentTime :-} [generateHelper Haskell name (T.unpack pName,edts) False]) places
                mapM_ (\(HybridPlace pName _ pEdts _ _ _ _) -> writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Helpers" </> T.unpack pName ++ "Player" <.> "hs") $ T.unlines $ {-disclaimer currentTime :-} [generateHelper Haskell name (T.unpack pName ++ "Player",pEdts) False]) places
                writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Encode" <.> "hs") encoder
                writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Decode" <.> "hs") decoder
                writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Wrappers" <.> "hs") wrappers
                writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Update" <.> "hs") hiddenUpdate
                writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "FromSuperPlace" <.> "hs") fromSuperPlace
                generatePlugins (fp </> "server" </> "src" </> T.unpack name) name plugins