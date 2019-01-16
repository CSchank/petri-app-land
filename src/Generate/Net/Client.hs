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

trans2constr :: NetTransition -> Constructor
trans2constr trans = 
    case trans of
        NetTransition constr _ _ -> constr

transName from msgN = T.concat [T.pack msgN,"from",from]

getPlaceState :: HybridPlace -> Constructor
getPlaceState p =
    case p of
        (HybridPlace n s _ _ _ _ _) -> (T.unpack n,s)


getPlayerState :: HybridPlace -> Constructor
getPlayerState p =
    case p of
        (HybridPlace n _ s _ _ _ _) -> (T.unpack n,s)


generate :: M.Map String ElmCustom -> FilePath -> Net -> IO ()
generate extraTypes fp net =
    case net of 
        (HybridNet name startingPlace places transitions plugins) ->
            let
                inits = T.unlines 
                    [
                    T.concat ["module ", name, ".Init exposing(..)"]
                    ,T.concat["import ",name,".Static.Types"]
                    , ""
                    , "-- the initial states of each place in this net"
                    , T.unlines $ map (generateNetInit extraTypes) places -- the initial places
                    ]
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
                    T.concat ["module ", name, ".Static.Types exposing(..)"]
                    , ""
                    , "-- the types of all places in the net"
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
                incomingMsgs :: [(String,Constructor)]
                incomingMsgs = concat $ map (\(_,NetTransition (n,_) lstTrans _) -> 
                                            mapMaybe (\(from,(to,mConstr)) -> case mConstr of 
                                                                                        Just msg -> Just (n,msg)
                                                                                        Nothing -> Nothing)
                                                                                          lstTrans) transitions
                incomingCM = map (\(_,(msgN,edts)) -> (msgN,edts)) incomingMsgs
                incomingMsg = ElmCustom "IncomingMessage" $ map (\(n,t) -> ("M"++n,t)) incomingCM
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
                    T.unlines $ map (generateType Elm False [DOrd,DEq,DShow]) $ transitionType trans

                transConstrs :: [Constructor]
                transConstrs = map (trans2constr . snd) transitions

                transType :: ElmCustom
                transType = ec "Transition" transConstrs

                generateNetTypes :: T.Text -> [HybridPlace] -> T.Text
                generateNetTypes netName places = 
                    let
                        placeModel = 
                            generateType Elm False [DOrd,DEq,DShow] $ 
                                ElmCustom (T.unpack netName) $ map (\(HybridPlace n _ _ _ _ _ _) -> (T.unpack n,[edt (ElmType $ T.unpack n) "" ""])) places
                        placeTypes = T.unlines $ map generatePlaceType places
                        generatePlaceType :: HybridPlace -> T.Text
                        generatePlaceType (HybridPlace name _ _ clientPlaceState _ _ _) =
                            T.unlines
                                [
                                    generateType Elm True [DOrd,DEq,DShow,DTypeable] $ ElmCustom (T.unpack name) [(T.unpack name, clientPlaceState)],""
                                ]
                        clientMsgType :: T.Text
                        clientMsgType =
                            let
                                
                            in
                                T.unlines $ map (\(_,msg@(msgN,edts)) -> generateType Elm True [DOrd,DEq,DShow] $ ElmCustom msgN [msg]) incomingMsgs
                                ++ [generateType Elm True [DOrd,DEq,DShow] incomingMsg]
                    in
                        T.unlines 
                            [
                                "-- place states"
                            ,   placeTypes
                            ,   "-- union place type"
                            ,   generateType Elm False [] $ ec "NetState" $ map (\placeName -> constructor ("S"++T.unpack placeName) [edt (ElmType $ T.unpack placeName) "" ""]) placeNames
                            ,   "-- outgoing server message types"
                            ,   clientMsgType
                            ,   "-- individual transition types"
                            ,   T.unlines $ map transitionTxt transitions
                            ,   "-- extra server types"
                            ,   T.unlines $ map (generateType Elm False [DOrd,DEq,DShow] . snd) $ M.toList extraTypes
                            ]

                update :: T.Text
                update =
                    let
                    in T.unlines
                    [
                        T.concat ["module ",name,".Update exposing(..)"]
                    ,   T.concat ["import ",name,".Static.Types"]
                    ,   T.concat ["import ",name,".Static.FromSuperPlace"]
                    ,   "import Utils.Utils"
                    ,   ""
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
                                        T.concat ["        (",T.intercalate "," $ map uncapitalize allStates,",",T.intercalate "," fromVars,") = update",transTxt," tld ",clientIdTxt,"(wrap",transTxt," trans) ",T.replicate (length allStates) "(fromJust $ TM.lookup places) ", T.intercalate " " $ map (\t -> T.concat ["(map snd ",uncapitalize t, "PlayerLst)"]) froms]
                            ,   T.concat["        newPlaces = ",T.intercalate " $ " $ map (\state -> T.concat["TM.insert ",uncapitalize state]) allStates, " places"]
                            ,   T.concat["        (newPlayers, clientMessages) = unzip $ map (process",transTxt,"Player ",T.intercalate " " fromVars,") (IM'.toList players)"]
                            ,   "    in"
                            ,   T.concat["        (newPlaces, newPlayers, clientMessages, ", if isJust mCmd then "Just cmd" else "Nothing",")" ]
                            ]
                        disconnectCase placeName = 
                            T.concat ["            P",placeName,"Player {} -> (flip TM.insert) places $ clientDisconnectFrom",placeName," fsp clientID (fromJust $ TM.lookup places) (wrap",placeName,"Player player)"]
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
                    ,   "           ,    playerStates = IM'.fromList newPlayers"
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
                            fnub $ map (\(_,(to,_)) -> to) connections
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
                    in T.unlines 
                        [
                            T.unlines singularStubs
                        ]
                hiddenInit = T.unlines 
                    [
                        T.concat ["module ",name,".Static.Init exposing(..)"]
                    ,   T.concat ["import ",name,".Init as Init"]
                    ,   T.concat ["import ",name,".Update as Update"]
                    ,   T.concat ["import ",name,".Static.Wrappers"]
                    ,   "init :: NetState"
                    ,   T.concat ["init = S",startingPlace," Init.init"]
                    ]
                encoder = T.unlines 
                    [
                        T.concat ["module ",name,".Static.Encode exposing(..)"]
                    ,   T.concat ["import ",name,".Static.Types\n"]
                    ,   "import Utils.Utils"
                    ,   "import Static.Types"
                    ,   generateEncoder Elm $ outgoingTransitions
                    ]
                outgoingClientTransitions = mapMaybe (\(tt,NetTransition (name,ets) _ _) -> if tt == HybridTransition || tt == ClientOnlyTransition then Just ("T"++name,ets) else Nothing) transitions
                outgoingTransitions = ElmCustom "OutgoingTransition" outgoingClientTransitions
                incomingClientTransitions = concat $ mapMaybe (\(tt,NetTransition (name,ets) clientTransitions _) -> if tt == HybridTransition || tt == ClientOnlyTransition then Just $ mapMaybe (\(from,(to,indivC)) -> if isJust indivC then indivC else Nothing) clientTransitions else Nothing) transitions
                incomingTransitions = ElmCustom "IncomingTransition" $ map (\(n,et) -> ("M"++n,et)) incomingClientTransitions
                decoder = T.unlines 
                    [
                        T.concat ["module ",name,".Static.Decode exposing(..)"]
                    ,   T.concat ["import ",name,".Static.Types\n"]
                    ,   "import Utils.Utils"
                    ,   generateDecoder Elm $ incomingTransitions
                    ]
                wrappers = 
                    T.unlines
                        [
                            T.concat ["module ",name,".Static.Wrappers exposing(..)"]
                        ,   T.concat ["import ",name,".Static.Types\n"]
                        ,   T.unlines $ map (createUnwrap Elm "ClientMessage" "M") incomingCM
                        ,   T.unlines $ map (createWrap (length places > 1) Elm "ClientMessage" "M") incomingCM
                        ,   T.unlines $ map (createTransitionUnwrap (length places > 1) Elm) transitions
                        ,   T.unlines $ map (createUnwrap Elm "Transition" "T") transConstrs
                        ,   T.unlines $ map (createWrap (length transitions > 1) Elm "Transition" "T") transConstrs
                        ]        
            in do
                createDirectoryIfMissing True $ fp </> "client" </> "src" </> T.unpack name
                createDirectoryIfMissing True $ fp </> "client" </> "src" </> T.unpack name </> "Static"
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Init" <.> "hs") inits 
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Types" <.> "hs") types
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Init" <.> "hs") hiddenInit
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Update" <.> "hs") update
                createDirectoryIfMissing True $ fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Helpers"
                mapM_ (\(HybridPlace pName edts _ _ _ _ _)  -> writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Helpers" </> T.unpack pName <.> "hs") $ T.unlines $ {-disclaimer currentTime :-} [generateHelper Elm name (T.unpack pName,edts) False]) places
                mapM_ (\(HybridPlace pName _ pEdts _ _ _ _) -> writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Helpers" </> T.unpack pName ++ "Player" <.> "hs") $ T.unlines $ {-disclaimer currentTime :-} [generateHelper Elm name (T.unpack pName ++ "Player",pEdts) False]) places
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Encode" <.> "hs") encoder
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Decode" <.> "hs") decoder
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Wrappers" <.> "hs") wrappers
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Update" <.> "hs") hiddenUpdate
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "FromSuperPlace" <.> "hs") fromSuperPlace