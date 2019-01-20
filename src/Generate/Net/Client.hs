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
        (HybridPlace n _ s _ _ _ _) -> (T.unpack n,s)

getPlaceName :: HybridPlace -> T.Text
getPlaceName p =
    case p of
        (HybridPlace n _ _ _ _ _ _) -> n


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
                    ,T.concat["import ",name,".Static.Types exposing(..)"]
                    , ""
                    , "-- the initial states of each place in this net"
                    , T.concat ["init : ",startingPlace]
                    , T.concat ["init = ",constr2Def extraTypes (getPlaceState $ getPlace startingPlace)]
                    ]
                placeNames = map (\(HybridPlace name _ _ _ _ _ _) -> name) places
                -- the functions that the user changes
                generateNetInit :: M.Map String ElmCustom -> HybridPlace -> T.Text
                generateNetInit extraTypes (HybridPlace name _ _ clientPlaceState mSubnet (mCmd,_) _) = 
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
                types = T.unlines 
                    [
                    T.concat ["module ", name, ".Static.Types exposing(..)"]
                    , ""
                    , "-- the types of all places in the net"
                    , generateNetTypes name places -- the initial places
                    ]
                transFromPlace :: T.Text -> [(HybridTransition,NetTransition)]
                transFromPlace place = 
                        filter (\(_,NetTransition _ lstTrans _) ->
                                any (\(from,_) -> from == place) lstTrans
                            ) transitions
                perPlaceTypes (HybridPlace placeName _ _ _ _ _ _) = 
                    let
                        transitions = transFromPlace placeName
                        transConstrs = map (\(_,NetTransition constr _ _) -> constr) transitions
                    in
                    T.unlines
                    [
                        T.concat["module ",name,".Static.Types.",placeName," exposing(..)"]
                    ,   T.concat["import ",name,".Static.Types exposing(..)"]
                    ,   ""
                    ,   generateType Elm False [] $ ec "Msg" transConstrs
                    ]
                perPlaceWrappers (HybridPlace placeName _ _ _ _ _ _) = 
                    let
                        transitions = transFromPlace placeName
                        transConstrs = map (\(_,NetTransition (n,ets) _ _) -> (T.unpack name++".Static.Types."++T.unpack placeName++"."++n,ets)) transitions
                        zippedTrans = zip transConstrs wOutgoingClientTransitions
                    in
                    T.unlines
                    [
                        T.concat["module ",name,".Static.Wrappers.",placeName," exposing(..)"]
                    ,   T.concat["import ",name,".Static.Types.",placeName," exposing(..)"]
                    ,   T.concat["import ",name,".Static.Types exposing(..)"]
                    ,   ""
                    ,   "unwrap : Msg -> OutgoingTransition"
                    ,   "unwrap msg ="
                    ,   "    case msg of"
                    ,   T.unlines $ map (\(tc,wtc) -> T.concat["        ",generatePattern tc," -> ",generatePattern wtc]) zippedTrans
                    ]
                perPlaceViews (HybridPlace placeName _ _ _ _ _ _) = 
                    let
                        transitions = transFromPlace placeName
                        transConstrs = map (\(_,NetTransition constr _ _) -> constr) transitions
                    in
                    T.unlines
                    [
                        T.concat["module ",name,".View.",placeName," exposing(..)"]
                    ,   T.concat["import ",name,".Static.Types.",placeName," exposing(Msg(..))"]
                    ,   T.concat["import ",name,".Static.Types exposing(",placeName,"(..))"]
                    ,   "import Html exposing(Html)"
                    ,   "import Debug exposing(todo)"
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
                incomingMsgs = concat $ map (\(_,NetTransition (n,_) lstTrans _) -> 
                                            mapMaybe (\(from,(to,mConstr)) -> case mConstr of 
                                                                                        Just msg -> Just (n,msg)
                                                                                        Nothing -> Nothing)
                                                                                          lstTrans) transitions
                incomingCM = map (\(_,(msgN,edts)) -> (msgN,edts)) incomingMsgs
                incomingMsg = ElmCustom "IncomingMessage" $ map (\(n,t) -> ("M"++n,t)) incomingCM

                transConstrs :: [Constructor]
                transConstrs = map (trans2constr . snd) transitions

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
                        
                        --allConnections = concat $ map (\(_, NetTransition (transName,transEt) connections mCmd) -> ((transName,transEt),connections)) transitions

                    in
                        T.unlines 
                            [
                                "-- place states"
                            ,   placeTypes
                            ,   "-- union place type"
                            ,   generateType Elm False [] $ ec "NetState" $ map (\placeName -> constructor ("S"++T.unpack placeName) [edt (ElmType $ T.unpack placeName) "" ""]) placeNames
                            ,   "-- server transition types"
                            ,   generateType Elm False [] outgoingTransitions
                            ,   T.unlines $ map (\(pl,etd) -> generateType Elm False [] $ ec pl [(pl,etd)]) outgoingClientTransitions
                            ,   "-- outgoing server message types"
                            ,   clientMsgType
                            ,   "-- extra server types"
                            ,   T.unlines $ map (generateType Elm False [DOrd,DEq,DShow] . snd) $ M.toList extraTypes
                            ]

                update :: T.Text
                update =
                    let
                        trUpFn :: (HybridTransition,NetTransition) -> T.Text
                        trUpFn (_, NetTransition (transName,_) connections _) =
                            let
                                conUpFn (from,(to,mConstr)) = 
                                    case mConstr of
                                        Just (n,et) -> 
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
                    in T.unlines
                    [
                        T.concat ["module ",name,".Update exposing(..)"]
                    ,   T.concat ["import ",name,".Static.Types exposing(..)"]
                    ,   T.concat ["import ",name,".Static.FromSuperPlace exposing(..)"]
                    ,   "import Utils.Utils"
                    ,   "import Debug exposing(todo)"
                    ,   ""
                    ,   T.unlines $ map trUpFn transitions
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
                        updateCase :: (HybridTransition, NetTransition) -> T.Text
                        updateCase (_, NetTransition (transName,_) connections _) =
                            let
                                connectionCase (from,(to,mConstr)) =
                                    case mConstr of 
                                        Just (n,t) -> Just $ T.concat ["        ("
                                                                      ,generatePattern ("M"++n,map (\_ -> edt ElmBool "_" "") t)
                                                                      ,", S",from," st)"
                                                                      ," -> (S",to
                                                                      ," <| update",from,T.pack n,to
                                                                      ," fsp (wrap",T.pack n," trans) st, Nothing)"
                                                                      ]
                                        _ -> Nothing
                            in
                                T.unlines $ mapMaybe connectionCase connections
                    in T.unlines
                    [
                        T.concat ["module ",name,".Static.Update exposing(..)"]
                    ,   T.concat ["import ",name,".Static.Types exposing(..)"]
                    ,   T.concat ["import ",name,".Static.Wrappers exposing(..)"]
                    ,   T.concat ["import ",name,".Static.FromSuperPlace exposing (FromSuperPlace)"]
                    ,   T.concat ["import ",name,".Update exposing(..)"]
                    ,   ""
                    ,   T.concat ["update : FromSuperPlace -> IncomingMessage -> NetState -> (NetState,Maybe (Cmd OutgoingTransition))"]
                    ,   T.concat ["update fsp trans state ="]
                    ,   "    case (trans,state) of"
                    ,   T.unlines $ map updateCase transitions
                    ,   if length places > 1 then "        _ -> (state, Nothing)" else ""
                    ]
            
                placeMap :: M.Map T.Text HybridPlace
                placeMap = M.fromList $ map (\(pl@(HybridPlace n _ _ _ _ _ _)) -> (n,pl)) places

                getPlace :: T.Text -> HybridPlace
                getPlace name = M.findWithDefault (HybridPlace "" [] [] [] Nothing (Nothing,Nothing) Nothing) name placeMap

                hiddenInit = T.unlines 
                    [
                        T.concat ["module ",name,".Static.Init exposing(..)"]
                    ,   T.concat ["import ",name,".Init as Init"]
                    ,   T.concat ["import ",name,".Static.Types exposing (NetState(..))"]
                    ,   T.concat ["import ",name,".Update as Update"]
                    ,   T.concat ["import ",name,".Static.Wrappers"]
                    ,   "init : NetState"
                    ,   T.concat ["init = S",startingPlace," Init.init"]
                    ]
                encoder = T.unlines 
                    [
                        T.concat ["module ",name,".Static.Encode exposing(..)"]
                    ,   T.concat ["import ",name,".Static.Types exposing(..)\n"]
                    ,   "import Utils.Utils exposing(..)"
                    ,   "import Static.Types"
                    ,   generateEncoder Elm outgoingTransitions
                    ,   "--extra types encoders"
                    ,   T.unlines $ map (generateEncoder Elm) $ M.elems extraTypes
                    ]
                outgoingClientTransitions = mapMaybe (\(tt,NetTransition constr _ _) -> if tt == HybridTransition || tt == ClientOnlyTransition then Just constr else Nothing) transitions
                wOutgoingClientTransitions = map (\(n,t) -> ("T"++n,t)) outgoingClientTransitions
                outgoingTransitions = ElmCustom "OutgoingTransition" wOutgoingClientTransitions
                incomingClientTransitions = concat $ mapMaybe (\(tt,NetTransition (name,ets) clientTransitions _) -> if tt == HybridTransition || tt == ClientOnlyTransition then Just $ mapMaybe (\(from,(to,indivC)) -> if isJust indivC then indivC else Nothing) clientTransitions else Nothing) transitions
                incomingTransitions = ElmCustom "IncomingMessage" $ map (\(n,et) -> ("M"++n,et)) incomingClientTransitions
                decoder = T.unlines 
                    [
                        T.concat ["module ",name,".Static.Decode exposing(..)"]
                    ,   T.concat ["import ",name,".Static.Types exposing(..)\n"]
                    ,   "import Utils.Utils exposing(..)"
                    ,   generateDecoder Elm $ incomingTransitions
                    ,   "--extra types decoders"
                    ,   T.unlines $ map (generateDecoder Elm) $ M.elems extraTypes
                    ]
                wrappers = 
                    T.unlines
                        [
                            T.concat ["module ",name,".Static.Wrappers exposing(..)"]
                        ,   T.concat ["import ",name,".Static.Types exposing(..)\n"]
                        ,   T.unlines $ map (createWrap extraTypes (length places > 1) Elm "IncomingMessage" "M") incomingCM
                        ,   T.unlines $ map (createUnwrap Elm "OutgoingTransition" "T") outgoingClientTransitions
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
                    ,   "view : NetState -> Html OutgoingTransition"
                    ,   "view ns ="
                    ,   "    case ns of"
                    ,   T.unlines $ map viewCase placeNames
                    ,   "title : NetState -> String"
                    ,   "title ns ="
                    ,   "    case ns of"
                    ,   T.unlines $ map titlCase placeNames
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
                mapM_ (\(HybridPlace pName _ _ edts _ _ _)  -> writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Helpers" </> T.unpack pName <.> "elm") $ T.unlines $ {-disclaimer currentTime :-} [generateHelper Elm name (T.unpack pName,edts) False]) places
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Encode" <.> "elm") encoder
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Decode" <.> "elm") decoder
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Wrappers" <.> "elm") wrappers
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "Update" <.> "elm") hiddenUpdate
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "FromSuperPlace" <.> "elm") fromSuperPlace
                writeIfNew 0 (fp </> "client" </> "src" </> T.unpack name </> "Static" </> "View" <.> "elm") hiddenView