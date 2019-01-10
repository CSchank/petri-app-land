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
                update :: T.Text
                update =
                    let
                        
                    in T.unlines
                    [
                        T.concat ["module ",name,".Update where"]
                    ,   T.concat ["import ",name,".Static.Types"]
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
                        processTransPlayer (tr@(_, NetTransition (transName,_) connections mCmd)) = T.unlines
                            [
                                T.concat["process",T.pack transName,"Player :: Player -> (Player, Maybe ClientMessage)"]
                            ,   T.concat["process",T.pack transName,"Player = case player of"]
                            ,   T.unlines $ map (\fromPlace -> 
                                        let
                                            placeState = getPlaceState $ getPlace $ fromPlace
                                        in
                                            T.concat ["    ",generatePattern placeState," -> unwrap",T.pack transName,"from",fromPlace]
                                            ) $ fst $ fromsTos tr
                            ]
                    in T.unlines
                    [
                        T.concat ["module ",name,".Static.Update where"]
                    ,   T.concat ["import ",name,".Static.Types"]
                    ,   ""
                    ,   T.unlines $ map processTransPlayer transitions
                    ,   T.concat ["update :: Transition -> NetState Player -> [(ClientID,Player)] -> (NetState Player,[ClientMessage],Maybe (Cmd Transition),[(ClientID,Player)])"]
                    ,   T.concat ["update trans state players ="]
                    ,   T.concat ["    let"]
                    ,   T.concat ["        processPlayer :: Player -> Player"]
                    ,   T.concat ["        processPlayer player = case player of"]
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
                        outputs = fnub $ placeInputs ++ placeOutputs ++ singularTransFns ++ cmds

                        singularTransFns :: [T.Text]
                        singularTransFns = map (\(from,lst) -> 
                                                let 
                                                    output = transName from msgN
                                                in
                                                    T.concat [from,"Player -> ",output]) (grouped connections)

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