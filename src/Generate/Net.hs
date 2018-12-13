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
                [("A", ("B", ToSender $ constructor "AtoB" [edt (ElmIntRange 0 1000) "n" ""]))]
                Nothing
        trans2 =
            NetTransition
                (constructor "CA" [edt (ElmIntRange 0 1000) "n" ""])
                [("C", ("A", ToSender $ constructor "CtoA" [edt (ElmIntRange 0 1000) "n" ""]))]
                Nothing
        trans3 =
            NetTransition
                (constructor "ABC" [edt (ElmIntRange 0 1000) "n" ""])
                [("A", ("B", ToSender $ constructor "CtoA" [edt (ElmIntRange 0 1000) "n" ""]))
                ,("A", ("C", ToSender $ constructor "CtoA" [edt (ElmIntRange 0 1000) "n" ""]))
                ]
                Nothing
        net = HybridNet
                "TestNet"
                [place1,place2,place3]
                [(HybridTransition,trans1),(HybridTransition,trans2),(HybridTransition,trans3)]
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
                                ElmCustom (T.unpack netName) $ map (\(HybridPlace n m _ _ _) -> ("P"++T.unpack n,[edt (ElmType $ T.unpack n) "" ""]) {-(T.unpack $ T.concat["P",n],m)-}) places
                        allFields :: [(ElmType, T.Text)]
                        allFields = S.toList $ S.fromList $
                                        concat $ map (\(HybridPlace _ edts _ _ _) -> map (\(et,n,_) -> (et,T.pack n)) edts) places
                        createClass :: (ElmType, T.Text) -> T.Text
                        createClass (et,f) = 
                            let
                                field = capitalize f
                                fieldType = et2Txt True True et
                            in
                            T.unlines 
                            [
                                T.concat ["class Has",field," a where"]
                            ,   T.concat ["    get",field," :: a -> ",fieldType]
                            ,   T.concat ["    update",field," :: ",fieldType, " -> a -> a"]
                            ,   T.concat ["    alter",field," :: (",fieldType, " -> ", fieldType,") -> a -> a"]
                            ]
                        createInstance :: Constructor -> ElmDocType -> T.Text
                        createInstance constr (et,n,d) = 
                            let
                                f = T.pack n
                                field = capitalize $ T.pack n
                                fieldType = et2Txt True True et 
                                newValue = (fst constr, map (\(ett,nn,dd) -> if nn == T.unpack f then (ett,"new"++nn,dd) else (ett,nn,dd)) $ snd constr)
                            in
                            T.unlines 
                            [
                                T.concat ["instance Has",field," ",T.pack $ fst constr," where"]
                            ,   T.concat ["    get",field," ",generatePattern constr," = ",f]
                            ,   T.concat ["    update",field," new",f," ",generatePattern constr," = ",generatePattern newValue]
                            ,   T.concat ["    alter",field," f ",generatePattern constr," = let new",f, " = f ",f," in ", generatePattern newValue]
                            ]
                        placeTypes = T.unlines $ map generatePlaceType places
                        generatePlaceType :: HybridPlace -> T.Text
                        generatePlaceType (HybridPlace name serverPlaceState playerPlaceState _ _) =
                            T.unlines
                                [
                                    generateType True True [DOrd,DEq,DShow] $ ElmCustom (T.unpack name) [(T.unpack name, serverPlaceState)],""
                                ,   T.unlines $ map (createInstance (T.unpack name,serverPlaceState)) serverPlaceState
                                ]
                    in
                        T.unlines 
                            [
                                T.unlines $ map createClass allFields   --classes for helpers
                            ,   placeModel,""
                            ,   placeTypes
                            ]
            in do
                createDirectoryIfMissing True $ fp </> T.unpack name </> "userApp"
                createDirectoryIfMissing True $ fp </> T.unpack name </> "Static"
                writeIfNew 0 (fp </> T.unpack name </> "userApp" </> "Init" <.> "hs") inits 
                writeIfNew 0 (fp </> T.unpack name </> "Static" </> "Types" <.> "hs") types
