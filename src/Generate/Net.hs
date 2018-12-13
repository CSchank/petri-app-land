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
import Data.Maybe (mapMaybe)
import Generate.Helpers

testNet =
    let
        place1 = 
            HybridPlace "A" 
                [edt (ElmIntRange 0 1000) "n" "",edt (ElmList $ edt (ElmIntRange 0 1000) "n" "") "nLst" ""] --server state
                [edt (ElmIntRange 0 1000) "playerN" ""] --player state
                [edt (ElmIntRange 0 1000) "n" ""]
                Nothing
        place2 = 
            HybridPlace "B" 
                [edt (ElmIntRange 0 1000) "n" "",edt (ElmList $ edt (ElmIntRange 0 1000) "n" "") "nLst" ""] --server state
                [edt (ElmIntRange 0 1000) "playerN" ""] --player state
                [edt (ElmIntRange 0 1000) "n" ""]
                Nothing
        place3 = 
            HybridPlace "C" 
                [edt (ElmIntRange 0 1000) "n" "",edt (ElmList $ edt (ElmIntRange 0 1000) "n" "") "nLst" ""] --server state
                [edt (ElmIntRange 0 1000) "playerN" ""] --player state
                [edt (ElmIntRange 0 1000) "n" ""]
                Nothing
        trans1 =
            NetTransition
                (constructor "AB" [edt (ElmIntRange 0 1000) "n" ""])
                [("A", ("B", Just $ constructor "AtoB" [edt (ElmIntRange 0 1000) "n" ""]))]
                Nothing
        trans2 =
            NetTransition
                (constructor "CA" [edt (ElmIntRange 0 1000) "n" ""])
                [("C", ("A", Just $ constructor "CtoA" [edt (ElmIntRange 0 1000) "n" ""]))]
                Nothing
        trans3 =
            NetTransition
                (constructor "ABC" [edt (ElmIntRange 0 1000) "n" ""])
                [("A", ("B", Just $ constructor "AtoB" [edt (ElmIntRange 0 1000) "n" ""]))
                ,("A", ("C", Just $ constructor "AtoC" [edt (ElmIntRange 0 1000) "n" ""]))
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
                generateNetInit extraTypes (HybridPlace name serverPlaceState playerPlaceState _ mSubnet) = 
                    let
                        fnName = T.concat ["init",name]
                    in
                        T.unlines 
                        [
                            T.concat [fnName, " :: ", capitalize name]
                        ,   T.concat [fnName, " = ", constr2Def extraTypes (T.unpack name,serverPlaceState)]
                        ]
                types = T.unlines 
                    [
                    T.concat ["module ", name, ".Types where"]
                    , ""
                    , generateNetTypes name places -- the initial places
                    ]
                generateNetTypes :: T.Text -> [HybridPlace] -> T.Text
                generateNetTypes netName places = 
                    let
                        placeModel = 
                            generateType True False [DOrd,DEq,DShow] $ 
                                ElmCustom (T.unpack netName) $ map (\(HybridPlace n m _ _ _) -> (T.unpack n++"Player",[edt (ElmType $ T.unpack n) "" ""])) places
                        placeTypes = T.unlines $ map generatePlaceType places
                        generatePlaceType :: HybridPlace -> T.Text
                        generatePlaceType (HybridPlace name serverPlaceState playerPlaceState _ _) =
                            T.unlines
                                [
                                    generateType True True [DOrd,DEq,DShow] $ ElmCustom (T.unpack name) [(T.unpack name, serverPlaceState)],""
                                ,   generateType True True [DOrd,DEq,DShow] $ ElmCustom (T.unpack name++"Player") [(T.unpack name++"Player",playerPlaceState)],""
                                ]
                        clientMsgType :: T.Text
                        clientMsgType =
                            let
                                msgs :: [(String,Constructor)]
                                msgs = concat $ map (\(_,NetTransition (n,_) lstTrans _) -> 
                                            mapMaybe (\(from,(to,mConstr)) -> case mConstr of 
                                                                                        Just msg -> Just (n,msg)
                                                                                        Nothing -> Nothing)
                                                                                          lstTrans) transitions
                            in
                                T.unlines $ map (\(_,msg@(msgN,edts)) -> generateType True True [DOrd,DEq,DShow] $ ElmCustom msgN [msg]) msgs
                                ++ [generateType True True [DOrd,DEq,DShow] $ ElmCustom "ClientMessage" $ map (\(qual,(msgN,edts)) -> (qual++"_"++msgN,edts)) msgs]
                    in
                        T.unlines 
                            [
                                placeTypes
                            ,   clientMsgType
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
                        placeMap = M.fromList $ map (\(pl@(HybridPlace n _ _ _ _)) -> (n,pl)) places

                        getPlace :: T.Text -> HybridPlace
                        getPlace name = M.findWithDefault (HybridPlace "" [] [] [] Nothing) name placeMap

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
                        outputs = placeInputs ++ placeOutputs ++ singularTransFns
                        grouped :: [(T.Text, [(T.Text, Maybe Constructor)])]
                        grouped = M.toList $ M.fromListWith (\ a b -> a ++ b) $ map (\(a,b) -> (a,[b])) connections

                        singularTransFns :: [T.Text]
                        singularTransFns = map (\(from,lst) -> 
                                                let 
                                                    output = T.concat [from,"to",T.intercalate "or" $ placeOutputs]
                                                in
                                                    T.concat [from," -> ",output]) grouped

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
                                                    output = T.concat [from,"to",T.intercalate "or" $ placeOutputs]
                                                    name = T.concat ["        from",from]
                                                    typ = T.concat [name, " :: ",from," -> ",output]
                                                in
                                                    T.unlines 
                                                    [
                                                        typ
                                                    ,   T.concat [name," p",uncapitalize from," = error \"Please fill in function stub.\""]
                                                    ]) grouped

                    in T.unlines 
                        [
                            typ
                        ,   decl
                        ,   "    let"
                        ,   T.unlines singularStubs
                        ,   "    in"
                        ,   T.concat["        ",T.concat ["(",T.intercalate ", " $ map uncapitalize (placeInputs ++ placeOutputs),", ",T.intercalate ", " $ map (\txt -> T.concat ["from",txt]) placeInputs,")"]]
                        ]
                    
            in do
                createDirectoryIfMissing True $ fp </> T.unpack name </> "userApp"
                createDirectoryIfMissing True $ fp </> T.unpack name </> "Static"
                writeIfNew 0 (fp </> T.unpack name </> "userApp" </> "Init" <.> "hs") inits 
                writeIfNew 0 (fp </> T.unpack name </> "Static" </> "Types" <.> "hs") types
                writeIfNew 0 (fp </> T.unpack name </> "userApp" </> "Update" <.> "hs") update
                createDirectoryIfMissing True $ fp </> T.unpack name </> "Static" </> "Helpers"
                mapM_ (\(HybridPlace pName edts _ _ _) -> writeIfNew 1 (fp </> T.unpack name </> "Static" </> "Helpers" </> T.unpack pName <.> "hs") $ T.unlines $ {-disclaimer currentTime :-} [generateHelper False (T.unpack pName,edts) False]) places

