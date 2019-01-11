{-# LANGUAGE OverloadedStrings #-}

module Generate.Net where

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

getPlaceState :: HybridPlace -> Constructor
getPlaceState p =
    case p of
        (HybridPlace n s _ _ _ _ _) -> (T.unpack n,s)


generateServerNet :: M.Map String ElmCustom -> FilePath -> Net -> IO ()
generateServerNet extraTypes fp net =
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
                    , ""
                    , "-- the initial state of all places in this net"
                    , generateNetTypes name places -- the initial places
                    ]
                clientMsgs :: [(String,Constructor)]
                clientMsgs = concat $ map (\(_,NetTransition (n,_) lstTrans _) -> 
                                            mapMaybe (\(from,(to,mConstr)) -> case mConstr of 
                                                                                        Just msg -> Just (n,msg)
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
                            fnub $ map (\(_,(to,_)) -> to) connections
                        constructors :: (T.Text, [(T.Text, Maybe Constructor)]) -> [Constructor]
                        constructors (from,toLst) =
                            map (\(to,mConstr) -> 
                                case mConstr of 
                                    Just (msgName,_) -> constructor (T.unpack $ T.concat[T.pack msgN,"_",from,"to",to]) [edt (ElmType $ T.unpack $ T.concat [to,"Player"]) "" "", edt (ElmType msgName) "" ""]
                                    Nothing          -> constructor (T.unpack $ T.concat[T.pack msgN,"_",from,"to",to]) [edt (ElmType $ T.unpack $ T.concat [to,"Player"]) "" ""]
                                        ) toLst
                    in
                        map (\(from,toLst) -> ElmCustom (T.unpack $ transName from msgN) $ constructors (from,toLst)) (grouped connections)
                transitionTxt :: (HybridTransition, NetTransition) -> T.Text
                transitionTxt trans =
                    T.unlines $ map (generateType True False [DOrd,DEq,DShow]) $ transitionType trans

                generateNetTypes :: T.Text -> [HybridPlace] -> T.Text
                generateNetTypes netName places = 
                    let
                        placeModel = 
                            generateType True False [DOrd,DEq,DShow] $ 
                                ElmCustom (T.unpack netName) $ map (\(HybridPlace n m _ _ _ _ _) -> (T.unpack n++"Player",[edt (ElmType $ T.unpack n) "" ""])) places
                        placeTypes = T.unlines $ map generatePlaceType places
                        generatePlaceType :: HybridPlace -> T.Text
                        generatePlaceType (HybridPlace name serverPlaceState playerPlaceState _ _ _ _) =
                            T.unlines
                                [
                                    generateType True True [DOrd,DEq,DShow,DTypeable] $ ElmCustom (T.unpack name) [(T.unpack name, serverPlaceState)],""
                                ,   generateType True True [DOrd,DEq,DShow,DTypeable] $ ElmCustom (T.unpack name++"Player") [(T.unpack name++"Player",playerPlaceState)],""
                                ]
                        playerUnionType = 
                            ElmCustom "Player" $ map (\(n,t) -> ("P"++n,t)) placePlayerStates
                        clientMsgType :: T.Text
                        clientMsgType =
                            let
                                
                            in
                                T.unlines $ map (\(_,msg@(msgN,edts)) -> generateType True True [DOrd,DEq,DShow] $ ElmCustom msgN [msg]) clientMsgs
                                ++ [generateType True True [DOrd,DEq,DShow] clientMsg]
                        transConstrs :: [Constructor]
                        transConstrs = map (trans2constr . snd) transitions
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
                            ,   generateType True True [DOrd,DEq,DShow] $ ec "Transition" $ map (\(n,t) -> ("T"++n,t)) transConstrs
                            ,   T.unlines $ map (\(n,et) -> generateType True True [DOrd,DEq,DShow] $ ec n [(n,et)]) transConstrs
                            ,   "-- player state union type"
                            ,   generateType True False [DOrd,DEq,DShow] playerUnionType
                            ]

                --singularTransFns :: [T.Text]
                singularTransFns msgN connections = map (\(from,lst) -> 
                                        let 
                                            output = transName from msgN
                                        in
                                            T.concat [from,"Player -> ",output]) (grouped connections)

                update :: T.Text
                update =
                    let
                        disconnectFn place = 
                            let
                                disconnectName = T.concat["clientDisconnectFrom",place]
                            in
                            T.unlines
                            [
                                    T.concat[disconnectName," :: ClientID -> ",place," -> ",place,"Player -> ",place]
                                ,   T.concat[disconnectName," clientID ",uncapitalize place," ",uncapitalize place,"Player ="]
                                ,   T.concat["    error \"Please fill out the ",disconnectName," function for the ",name," net.\""]
                            ]
                    in T.unlines
                    [
                        T.concat ["module ",name,".Update where"]
                    ,   T.concat ["import ",name,".Static.Types"]
                    ,   ""
                    ,   "-- function called when new client connects (do not delete)"
                    ,   T.concat["clientConnect :: ClientID -> ",startingPlace," -> (", startingPlace,", ",startingPlace,"Player)"]
                    ,   T.concat["clientConnect clientID ",uncapitalize startingPlace," ="]
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
                    (fnub $ map (\(from,(to,_)) -> from) connections,fnub $ map (\(from,(to,_)) -> to) connections)
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
                                T.concat["process",T.pack transName,"Player :: ",T.intercalate " -> " $ tfns," -> Player -> (Player, Maybe ClientMessage)"]
                            ,   T.concat["process",T.pack transName,"Player ",T.intercalate " " froms," player = case player of"]
                            ,   T.unlines $ map (\fromPlace -> 
                                        let
                                            placeState = getPlaceState $ getPlace $ fromPlace
                                        in
                                            T.concat ["    ",generatePattern placeState," -> unwrap",T.pack transName,"from",fromPlace," $ from",fromPlace," $ wrap",fromPlace,"Player player"]
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
                                T.concat["split",T.pack transName,"Players :: [Player] -> (",T.intercalate "," $ map (\p -> T.concat["[",p,"Player]"]) $ fst $ fromsTos tr,")"]
                            ,   T.concat["split",T.pack transName,"Players players = foldl (\\t@",foldTuple," pl -> case pl of"]
                            ,   T.unlines $ map (\(fromPlace,n) -> 
                                        let
                                            placeState = getPlaceState $ getPlace $ fromPlace
                                        in
                                            T.concat ["    P",fromPlace,"Player {} -> (" ,T.intercalate "," $ map (\(f,m) -> if n == m then T.concat["unwrapFrom", fromPlace," pl:",f,"lst"] else T.concat[f,"lst"]) $ zip froms [0..],")"]
                                            ) $ zip (fst $ fromsTos tr) [0..]
                            ,   T.concat ["    _ -> t) (",T.intercalate "," $ replicate (length froms) "[]",") players"]
                            ]
                        transCase tr@(_, NetTransition constr@(transName,transArgs) _ mCmd) = 
                            let
                                froms = fst $ fromsTos tr
                                tos = snd $ fromsTos tr
                                allStates = fnub $ froms ++ tos
                                fromVars = map (\t -> T.concat["from",t]) froms
                                transTxt = T.pack transName
                            in
                            T.unlines $ map (\t -> T.concat["                ",t]) $
                            [
                                T.concat [generatePattern ("T"++transName,transArgs),  "->"]
                            ,   "    let"
                            ,   T.concat["        (",T.intercalate "," $ map (\t -> T.concat[uncapitalize t,"PlayerLst"]) froms,") = split",transTxt,"Players players"]
                            ,   case mCmd of
                                    Just cmd -> 
                                        T.concat ["        (",T.intercalate "," $ map uncapitalize allStates,",",T.intercalate "," fromVars,",cmd) = update",transTxt," ",T.replicate (length allStates) "(fromJust $ TM.lookup places) ", T.intercalate " " $ map (\t -> T.concat [uncapitalize t, "PlayerLst"]) froms]
                                    Nothing ->
                                        T.concat ["        (",T.intercalate "," $ map uncapitalize allStates,",",T.intercalate "," fromVars,") = update",transTxt," ",T.replicate (length allStates) "(fromJust $ TM.lookup places) ", T.intercalate " " $ map (\t -> T.concat [uncapitalize t, "PlayerLst"]) froms]
                            ,   T.concat["        newPlaces = ",T.intercalate " $ " $ map (\state -> T.concat["TM.insert ",uncapitalize state]) froms, " places"]
                            ,   T.concat["        (newPlayers, clientMessages) = unzip $ map (process",transTxt,"Player ",T.intercalate " " fromVars,") players"]
                            ,   "    in"
                            ,   T.concat["        (newPlaces, newPlayers, clientMessages, ", if isJust mCmd then "Just cmd" else "Nothing",")" ]
                            ]
                    in T.unlines
                    [
                        T.concat ["module ",name,".Static.Update where"]
                    ,   T.concat ["import ",name,".Static.Types"]
                    ,   T.concat ["import ",name,".Static.Wrappers"]
                    ,   "import Data.TMap as TM"
                    ,   ""
                    ,   "-- player processing functions"
                    ,   T.unlines $ map processTransPlayer transitions
                    ,   "-- player splitting functions"
                    ,   T.unlines $ map splitPlayers transitions
                    ,   T.concat ["update :: Transition -> NetState Player -> (NetState Player,[ClientMessage],Maybe (Cmd Transition))"]
                    ,   T.concat ["update trans state ="]
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
                    ,   "           ,    playerStates = newPlayers"
                    ,   "           }"
                    ,   "        , clientMessages"
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
                            fnub $ map (\(_,(to,_)) -> to) connections
                        clientIdType = case transType of 
                            HybridTransition -> "Maybe ClientID ->"
                            ClientOnlyTransition -> "ClientID ->"
                            ServerOnlyTransition -> ""
                        
                        clientId = case transType of 
                            HybridTransition -> "mClientId"
                            ClientOnlyTransition -> "clientId"
                            ServerOnlyTransition -> ""

                        fnName = T.concat["update",T.pack msgN]
                        outputs = fnub $ placeInputs ++ placeOutputs ++ (singularTransFns msgN connections) ++ cmds

                        oneOfs = T.concat $ "OneOf" : map (\txt -> T.concat[txt,"Player"]) placeOutputs
                        typ = T.concat  [ fnName," :: "
                                        , clientIdType," "
                                        , T.pack msgN," -> "
                                        , T.intercalate " -> " (fnub $ placeInputs ++ placeOutputs ++ map (\txt -> T.concat["List ",txt,"Player"]) placeInputs), " -> "
                                        , T.concat ["(",T.intercalate ", " outputs,")"]
                                        ] 
                        
                        decl = T.concat [ fnName," "
                                        , clientId," "
                                        , pattern
                                        , T.intercalate " " $ fnub $ map uncapitalize $ (placeInputs ++ placeOutputs ++ map (\txt -> T.concat["lst",txt]) placeInputs), " ="
                                        ] 
                        singularStubs = 
                                        map (\(from,lst) -> 
                                                let 
                                                    oneOfs = map (\(to,mConstr) -> case mConstr of 
                                                        Just (msg,_) -> T.concat["(P",to,", ",T.pack msg,")"]
                                                        Nothing  -> T.concat[to,"Player"]
                                                        ) lst
                                                    output = transName from msgN
                                                    name = T.concat ["        from",from]
                                                    typ = T.concat [name, " :: ",from,"Player -> ",output]
                                                in
                                                    T.unlines 
                                                    [
                                                        typ
                                                    ,   T.concat [name," p",uncapitalize from," = error \"Please fill in function stub.\""]
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
                    ,   "import Data.TMap as TM\n"
                    ,   T.concat["init :: NetState Player"]
                    ,   "init = NetState"
                    ,   "    {"
                    ,   "      playerStates = IM'.empty"
                    ,   T.concat["    , placeStates = ",T.concat $ map (\(HybridPlace name _ _ _ _ (mCmd,_) _) -> T.concat["TM.insert",if isJust mCmd then T.concat[" (fst init",name] else T.concat[" init",name]," $ "]) places,"TM.empty"]
                    ,   T.concat["    , pluginStates = TM.empty"]
                    ,   "    }"
                    ]
                encoder = T.unlines 
                    [
                        T.concat ["module ",name,".Encode where"]
                    ,   T.concat ["import ",name,".Static.Types\n"]
                    ,   generateEncoder True clientMsg
                    ]
                incomingClientTransitions = mapMaybe (\(tt,NetTransition constr _ _) -> if tt == HybridTransition || tt == ClientOnlyTransition then Just constr else Nothing) transitions
                clientTransitions = ElmCustom "IncomingMessage" incomingClientTransitions
                decoder = T.unlines 
                    [
                        T.concat ["module ",name,".Decode where"]
                    ,   T.concat ["import ",name,".Static.Types\n"]
                    ,   generateDecoder True clientTransitions
                    ]
                wrappers = 
                    T.unlines
                        [
                            T.concat ["module ",name,".Static.Wrappers where"]
                        ,   T.concat ["import ",name,".Static.Types\n"]
                        ,   T.unlines $ map (createUnwrap True "ClientMessage" "M") outgoingCM
                        ,   T.unlines $ map (createWrap (length places > 1) True "ClientMessage" "M") outgoingCM
                        ,   T.unlines $ map (createUnwrap True "Player" "P") placePlayerStates
                        ,   T.unlines $ map (createWrap (length places > 1) True "Player" "P") placePlayerStates
                        ,   T.unlines $ map (createTransitionUnwrap (length places > 1) True) transitions
                        ]        
            in do
                createDirectoryIfMissing True $ fp </> "server" </> "src" </> T.unpack name </> "userApp"
                createDirectoryIfMissing True $ fp </> "server" </> "src" </> T.unpack name </> "Static"
                writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "userApp" </> "Init" <.> "hs") inits 
                writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Types" <.> "hs") types
                writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Init" <.> "hs") hiddenInit
                writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "userApp" </> "Update" <.> "hs") update
                createDirectoryIfMissing True $ fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Helpers"
                mapM_ (\(HybridPlace pName edts _ _ _ _ _)  -> writeIfNew 1 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Helpers" </> T.unpack pName <.> "hs") $ T.unlines $ {-disclaimer currentTime :-} [generateHelper True (T.unpack pName,edts) False]) places
                mapM_ (\(HybridPlace pName _ pEdts _ _ _ _) -> writeIfNew 1 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Helpers" </> T.unpack pName ++ "Player" <.> "hs") $ T.unlines $ {-disclaimer currentTime :-} [generateHelper True (T.unpack pName ++ "Player",pEdts) False]) places
                writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Encode" <.> "hs") encoder
                writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Decode" <.> "hs") decoder
                writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Wrappers" <.> "hs") wrappers
                writeIfNew 0 (fp </> "server" </> "src" </> T.unpack name </> "Static" </> "Update" <.> "hs") hiddenUpdate
                generatePlugins (fp </> "server" </> "src" </> T.unpack name) plugins