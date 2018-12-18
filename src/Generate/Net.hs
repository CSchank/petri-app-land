{-# LANGUAGE OverloadedStrings #-}

module Generate.Net where

import Types
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S
import Generate.Types
import TypeHelpers
import Utils
import                  System.FilePath.Posix   ((</>),(<.>))
import System.Directory
import Data.Maybe (mapMaybe, isJust)
import Generate.Helpers
import Generate.Codec
import Generate.Wrappers

testNet =
    let
        place1 = 
            HybridPlace "A" 
                [edt (ElmIntRange 0 1000) "n" "",edt (ElmList $ edt (ElmIntRange 0 1000) "n" "") "nLst" ""] --server state
                [edt (ElmIntRange 0 1000) "playerN" ""] --player state
                [edt (ElmIntRange 0 1000) "n" ""]
                Nothing
                (Nothing, Nothing)
                Nothing

        place2 = 
            HybridPlace "B" 
                [edt (ElmIntRange 0 1000) "n" "",edt (ElmList $ edt (ElmIntRange 0 1000) "n" "") "nLst" ""] --server state
                [edt (ElmIntRange 0 1000) "playerN" ""] --player state
                [edt (ElmIntRange 0 1000) "n" ""]
                Nothing
                (Nothing, Nothing)
                Nothing

        place3 = 
            HybridPlace "C" 
                [edt (ElmIntRange 0 1000) "n" "",edt (ElmList $ edt (ElmIntRange 0 1000) "n" "") "nLst" ""] --server state
                [edt (ElmIntRange 0 1000) "playerN" ""] --player state
                [edt (ElmIntRange 0 1000) "n" ""]
                Nothing
                (Nothing, Nothing)
                Nothing
        trans1 =
            NetTransition
                (constructor "AB" [edt (ElmIntRange 0 1000) "n" ""])
                [("A", ("B", Just $ constructor "StartGameAB" [edt (ElmIntRange 0 1000) "n" ""]))]
                Nothing
        trans2 =
            NetTransition
                (constructor "CA" [edt (ElmIntRange 0 1000) "n" ""])
                [("C", ("A", Just $ constructor "StartGameCA" [edt (ElmIntRange 0 1000) "n" ""]))]
                Nothing
        trans3 =
            NetTransition
                (constructor "ABC" [edt (ElmIntRange 0 1000) "n" ""])
                [("A", ("B", Just $ constructor "StartGameAB2" [edt (ElmIntRange 0 1000) "n" ""]))
                ,("A", ("C", Just $ constructor "StartGameAC" [edt (ElmIntRange 0 1000) "n" ""]))
                ]
                Nothing
        net = HybridNet
                "TestNet"
                [place1,place2,place3]
                [(ServerOnlyTransition,trans1),(ClientOnlyTransition,trans2),(HybridTransition,trans3)]
    in
        generateServerNet M.empty "testnet" net


generateServerNet :: M.Map String ElmCustom -> FilePath -> Net -> IO ()
generateServerNet extraTypes fp net =
    case net of 
        (HybridNet name places transitions) ->
            let
                inits = T.unlines 
                    [
                    T.concat ["module ", name, ".Init where"]
                    ,T.concat["import ",name,".Static.Types"]
                    , ""
                    , "-- the initial states of each place in this net"
                    , T.unlines $ map (generateNetInit extraTypes) places -- the initial places
                    ]
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
                    , generateNetTypes name places -- the initial places
                    ]
                clientMsgs :: [(String,Constructor)]
                clientMsgs = concat $ map (\(_,NetTransition (n,_) lstTrans _) -> 
                                            mapMaybe (\(from,(to,mConstr)) -> case mConstr of 
                                                                                        Just msg -> Just (n,msg)
                                                                                        Nothing -> Nothing)
                                                                                          lstTrans) transitions
                outgoingCM = map (\(_,(msgN,edts)) -> (msgN,edts)) clientMsgs
                clientMsg = ElmCustom "ClientMessage" $ map (\(n,t) -> ("M",t)) outgoingCM
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
                        clientMsgType :: T.Text
                        clientMsgType =
                            let
                                
                            in
                                T.unlines $ map (\(_,msg@(msgN,edts)) -> generateType True True [DOrd,DEq,DShow] $ ElmCustom msgN [msg]) clientMsgs
                                ++ [generateType True True [DOrd,DEq,DShow] clientMsg]
                        transitionType :: (HybridTransition,NetTransition) -> T.Text
                        transitionType (transType, NetTransition (msgN,msg) connections mCmd) =
                            let
                                grouped :: [(T.Text, [(T.Text, Maybe Constructor)])]
                                grouped = M.toList $ M.fromListWith (\ a b -> a ++ b) $ map (\(a,b) -> (a,[b])) connections        
                                
                                output = T.concat [T.pack msgN,"Transition"]
                                placeInputs :: [T.Text]
                                placeInputs = 
                                    S.toList $ S.fromList $ map (\(from,_) -> from) connections
                                placeOutputs :: [T.Text]
                                placeOutputs = 
                                    S.toList $ S.fromList $ map (\(_,(to,_)) -> to) connections
                                constructors :: (T.Text, [(T.Text, Maybe Constructor)]) -> [Constructor]
                                constructors (from,toLst) =
                                    map (\(to,mConstr) -> 
                                        case mConstr of 
                                            Just (msgName,_) -> constructor (T.unpack $ T.concat[T.pack msgN,"_",from,"to",to]) [edt (ElmType $ T.unpack $ T.concat [to,"Player"]) "" "", edt (ElmType msgName) "" ""]
                                            Nothing          -> constructor (T.unpack $ T.concat[T.pack msgN,"_",from,"to",to]) [edt (ElmType $ T.unpack $ T.concat [to,"Player"]) "" ""]
                                                ) toLst
                            in
                                T.unlines $
                                        map (\(from,toLst) -> generateType True False [DOrd,DEq,DShow] $ ElmCustom (T.unpack output) $ constructors (from,toLst)) grouped
                    in
                        T.unlines 
                            [
                                placeTypes
                            ,   clientMsgType
                            ,   T.unlines $ map transitionType transitions
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
                generateTrans :: (HybridTransition,NetTransition) -> T.Text
                generateTrans (transType, NetTransition (msgN,msg) connections mCmd) =
                    let
                        placeMap :: M.Map T.Text HybridPlace
                        placeMap = M.fromList $ map (\(pl@(HybridPlace n _ _ _ _ _ _)) -> (n,pl)) places

                        getPlace :: T.Text -> HybridPlace
                        getPlace name = M.findWithDefault (HybridPlace "" [] [] [] Nothing (Nothing,Nothing) Nothing) name placeMap

                        pattern = generatePattern (msgN,msg)

                        placeInputs :: [T.Text]
                        placeInputs = 
                            S.toList $ S.fromList $ map (\(from,_) -> from) connections
                        placeOutputs :: [T.Text]
                        placeOutputs = 
                            S.toList $ S.fromList $ map (\(_,(to,_)) -> to) connections
                        clientIdType = case transType of 
                            HybridTransition -> "Maybe ClientID ->"
                            ClientOnlyTransition -> "ClientID ->"
                            ServerOnlyTransition -> ""
                        
                        clientId = case transType of 
                            HybridTransition -> "mClientId"
                            ClientOnlyTransition -> "clientId"
                            ServerOnlyTransition -> ""

                        fnName = T.concat["update",T.pack msgN]
                        outputs = placeInputs ++ placeOutputs ++ singularTransFns ++ cmds
                        grouped :: [(T.Text, [(T.Text, Maybe Constructor)])]
                        grouped = M.toList $ M.fromListWith (\ a b -> a ++ b) $ map (\(a,b) -> (a,[b])) connections

                        singularTransFns :: [T.Text]
                        singularTransFns = map (\(from,lst) -> 
                                                let 
                                                    output = T.concat [T.pack msgN,"Transition"]
                                                in
                                                    T.concat [from,"Player -> ",output]) grouped

                        oneOfs = T.concat $ "OneOf" : map (\txt -> T.concat[txt,"Player"]) placeOutputs
                        typ = T.concat  [ fnName," :: "
                                        , clientIdType," "
                                        , T.pack msgN," -> "
                                        , T.intercalate " -> " (placeInputs ++ placeOutputs ++ map (\txt -> T.concat["List ",txt,"Player"]) placeInputs), " -> "
                                        , T.concat ["(",T.intercalate ", " outputs,")"]
                                        ] 
                        
                        decl = T.concat [ fnName," "
                                        , clientId," "
                                        , pattern
                                        , T.intercalate " " $ map uncapitalize $ (placeInputs ++ placeOutputs ++ map (\txt -> T.concat["lst",txt]) placeInputs), " ="
                                        ] 
                        singularStubs = 
                                        map (\(from,lst) -> 
                                                let 
                                                    oneOfs = map (\(to,mConstr) -> case mConstr of 
                                                        Just (msg,_) -> T.concat["(P",to,", ",T.pack msg,")"]
                                                        Nothing  -> T.concat[to,"Player"]
                                                        ) lst
                                                    output = T.concat [T.pack msgN,"Transition"]
                                                    name = T.concat ["        from",from]
                                                    typ = T.concat [name, " :: ",from,"Player -> ",output]
                                                in
                                                    T.unlines 
                                                    [
                                                        typ
                                                    ,   T.concat [name," p",uncapitalize from," = error \"Please fill in function stub.\""]
                                                    ]) grouped
                        cmds = 
                            case mCmd of 
                                Just (msgN,msg) -> [T.pack msgN]
                                Nothing -> []

                    in T.unlines 
                        [
                            typ
                        ,   decl
                        ,   "    let"
                        ,   T.unlines singularStubs
                        ,   "    in"
                        ,   T.concat["        ",T.concat ["(",T.intercalate ", " $ map uncapitalize (placeInputs ++ placeOutputs),", "
                                                         ,T.intercalate ", " $ map (\txt -> T.concat ["from",txt]) placeInputs
                                                         ,T.replicate (length cmds) ", Cmd.none"
                                                         ,")"]
                                                         ]
                                    ]
                hiddenInit = T.unlines 
                    [
                        T.concat ["module ",name,".Static.Init where"]
                    ,   T.concat ["import ",name,".Static.Types"]
                    ,   T.concat ["import ",name,".Init"]
                    ,   "import Data.TMap as TM\n"
                    ,   "-- Initialize a TMap of the places in this Net"
                    ,   "init :: TMap"
                    ,   T.concat ["init = ",T.concat $ map (\(HybridPlace name _ _ _ _ (mCmd,_) _) -> T.concat["TM.insert",if isJust mCmd then "(fst init) " else "init ",name," $ "]) places,"TM.empty"]
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
                        ,   T.unlines $ map (createUnwrap True "ClientMessage" "M") outgoingCM
                        ]
                                
            in do
                createDirectoryIfMissing True $ fp </> T.unpack name </> "userApp"
                createDirectoryIfMissing True $ fp </> T.unpack name </> "Static"
                writeIfNew 0 (fp </> T.unpack name </> "userApp" </> "Init" <.> "hs") inits 
                writeIfNew 0 (fp </> T.unpack name </> "Static" </> "Types" <.> "hs") types
                writeIfNew 0 (fp </> T.unpack name </> "Static" </> "Init" <.> "hs") hiddenInit
                writeIfNew 0 (fp </> T.unpack name </> "userApp" </> "Update" <.> "hs") update
                createDirectoryIfMissing True $ fp </> T.unpack name </> "Static" </> "Helpers"
                mapM_ (\(HybridPlace pName edts _ _ _ _ _) -> writeIfNew 1 (fp </> T.unpack name </> "Static" </> "Helpers" </> T.unpack pName <.> "hs") $ T.unlines $ {-disclaimer currentTime :-} [generateHelper False (T.unpack pName,edts) False]) places
                writeIfNew 0 (fp </> T.unpack name </> "Static" </> "Encode" <.> "hs") encoder
                writeIfNew 0 (fp </> T.unpack name </> "Static" </> "Decode" <.> "hs") decoder
                writeIfNew 0 (fp </> T.unpack name </> "Static" </> "Wrappers" <.> "hs") wrappers