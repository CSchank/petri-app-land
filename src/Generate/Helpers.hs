{-# LANGUAGE OverloadedStrings #-}


module Generate.Helpers where

import Types
import qualified Data.Text as T
import                  System.FilePath.Posix   ((</>),(<.>))
import Generate.Types
import Utils
import                  Data.Time               (getCurrentTime)

generateHelpers :: FilePath -> T.Text -> [Constructor] -> [Constructor] -> IO ()
generateHelpers fp netName cStates sStates = do
    currentTime <- getCurrentTime
    mapM_ (\(sn,dt) -> writeIfNew 1 (fp </> "server" </> "src" </> "Static" </> "Helpers" </> T.unpack sn <.> "hs") $ T.unlines $ disclaimer currentTime : [generateHelper Haskell netName (sn,dt) False]) sStates
    mapM_ (\(sn,dt) -> writeIfNew 1 (fp </> "client" </> "src" </> "Static" </> "Helpers" </> T.unpack sn <.> "elm") $ T.unlines $ disclaimer currentTime : [generateHelper Elm netName (sn,dt) False]) cStates
    mapM_ (\(sn,dt) -> writeIfNew 1 (fp </> "client" </> "src" </> "Static" </> "Helpers" </> T.unpack sn ++ "Model" <.> "elm") $ T.unlines $ disclaimer currentTime : [generateHelper Elm netName (sn,dt) True]) cStates

generateHelper :: Language -> T.Text -> Constructor -> Bool -> T.Text
generateHelper l netName (snTxt,edts) getOnly =
    let
        (.::.) t0 t1 = if l == Haskell then T.concat [t0," :: ",t1] else T.concat[t0," : ",t1]
        generateGetter (et,n,_) =
            let
                nTxt = n
                fnName = T.concat["get",capitalize nTxt]
                newEdts = map (\(et,nn,dd) -> if nn == n then (et,nn,dd) else (et,"_",dd)) edts 
            in T.unlines
                [
                    T.concat [fnName .::. if getOnly then "Model" else snTxt," -> ",et2Txt l False et]
                ,   T.concat [fnName," ",generatePattern (if getOnly then T.concat["Static.Types.",snTxt,".",snTxt] else snTxt,newEdts)," = ",nTxt]
                ]
        generateSetter (et,nTxt,_) =
            let
                fnName = T.concat["update",capitalize nTxt]
                newValue = map (\(ett,nn,dd) -> if nn == nTxt then (ett,T.concat["new",nn],dd) else (ett,nn,dd)) edts
            in T.unlines
                [
                    T.concat [fnName .::. et2Txt l False et, " -> ",snTxt, " -> ",snTxt]
                ,   T.concat [fnName," new",nTxt," ",generatePattern (snTxt,edts)," = ", generatePattern (snTxt,newValue)]
                ]
        generateUpdater (et,nTxt,_) =
            let
                fnName = T.concat["alter",capitalize nTxt]
                newValue = map (\(ett,nn,dd) -> if nn == nTxt then (ett,T.concat["new",nn],dd) else (ett,nn,dd)) edts
            in T.unlines
                [
                    T.concat [fnName .::. "(",et2Txt l False et," -> ",et2Txt l False et,") -> ",snTxt," -> ",snTxt]
                ,   T.concat [fnName," f ",generatePattern (snTxt,edts)," = "]
                ,   T.concat ["    let"]
                ,   T.concat ["        new",nTxt," = f ",nTxt]
                ,   T.concat ["    in"]
                ,   T.concat ["        ",generatePattern (snTxt,newValue)]
                ]

        imports = fnub $ concatMap (findImports l) edts

    in
        T.unlines 
            ([
                if l == Haskell then T.concat ["module ",netName,".Static.Helpers.",snTxt," where"]
                else      T.concat ["module ",netName,".Static.Helpers.",snTxt,if getOnly then "Model" else ""," exposing (..)\nimport Dict exposing (Dict)"]
            ,   if getOnly then 
                    T.concat ["import Static.Types.",snTxt," exposing(Model(..))"] 
                else ""
            ,   if l == Elm then T.concat["import ",netName,".Static.ExtraTypes exposing(..)"] else ""
            ,   T.concat ["import ",netName,".Static.Types",if not $ l == Haskell then " exposing(..)" else ""]
            ,   T.concat ["import ",netName,".Static.Types"]
            ,   T.unlines imports
            ,   if l == Haskell then "import Static.List" else ""
            ,   T.unlines $ map generateGetter edts
            ,   if length edts == 0 then "x = Nothing" else ""
            ] ++
                if not getOnly then  
                [
                    T.unlines $ map generateSetter edts
                ,   T.unlines $ map generateUpdater edts
                ] else [])