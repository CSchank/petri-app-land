{-# LANGUAGE OverloadedStrings #-}

module Generate.Net where

import Types
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S
import Generate.Types
import Utils
import                  System.FilePath.Posix   ((</>),(<.>))


generateNet :: M.Map String ElmCustom -> FilePath -> Net -> IO ()
generateNet extraTypes fp (Net name netType places transitions) =
    let
        inits = T.unlines 
            [
              T.concat ["module ", name, ".Init where"]
            , ""
            , "-- the initial states of each place in this net"
            , T.unlines $ map (generateNetInit extraTypes) places -- the initial places
            ]
        types = T.unlines 
            [
              T.concat ["module ", name, ".Types where"]
            , ""
            , generateNetTypes name places -- the initial places
            ]
    in do
        writeIfNew 0 (fp </> T.unpack name </> "userApp" </> "Init" <.> "hs") inits 
        writeIfNew 0 (fp </> T.unpack name </> "Static" </> "Types" <.> "hs") types

-- the functions that the user changes
-- FIXME: needs to know about extra user types
generateNetInit :: M.Map String ElmCustom -> Place -> T.Text
generateNetInit extraTypes (Place name placeState playerPlaceState mSubnet) = 
    let
        fnName = T.concat ["init",name]
    in
        T.unlines 
        [
            T.concat [fnName, " :: ", capitalize name]
        ,   T.concat [fnName, " = ", constr2Def extraTypes (T.unpack name,placeState)]
        ]

generateNetTypes :: T.Text -> [Place] -> T.Text
generateNetTypes netName places = 
    let
        placeModel = 
            generateType True True [DOrd,DEq,DShow] $ 
                ElmCustom (T.unpack netName) $ map (\(Place n m _ _) -> (T.unpack $ T.concat["P",n],m)) places
        allFields :: [(ElmType, T.Text)]
        allFields = S.toList $ S.fromList $
                        concat $ map (\(Place _ edts _ _) -> map (\(et,n,_) -> (et,T.pack n)) edts) places
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
    in
        T.unlines 
            [
                T.unlines $ map createClass allFields   --classes for helpers
            ,   placeModel
            ]
