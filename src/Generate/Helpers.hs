{-# LANGUAGE OverloadedStrings #-}


module Generate.Helpers where

import Types
import qualified Data.Text as T
import                  System.FilePath.Posix   ((</>),(<.>))
import Generate.Types
import Data.Char (toUpper)
import Utils
import                  Data.Time               (getCurrentTime)

generateHelpers :: FilePath -> [Constructor] -> [Constructor] -> IO ()
generateHelpers fp cStates sStates = do
    currentTime <- getCurrentTime
    mapM_ (\(sn,edt) -> writeIfNew (fp </> "server" </> "src" </> "Static" </> "Helpers" </> sn <.> "hs") $ T.unlines $ disclaimer currentTime : [generateHelper True (sn,edt) False]) sStates
    mapM_ (\(sn,edt) -> writeIfNew (fp </> "client" </> "src" </> "Static" </> "Helpers" </> sn <.> "elm") $ T.unlines $ disclaimer currentTime : [generateHelper False (sn,edt) False]) cStates
    mapM_ (\(sn,edt) -> writeIfNew (fp </> "client" </> "src" </> "Static" </> "Helpers" </> sn ++ "Model" <.> "elm") $ T.unlines $ disclaimer currentTime : [generateHelper False (sn,edt) True]) cStates

generateHelper :: Bool -> Constructor -> Bool -> T.Text
generateHelper h (sn,edts) getOnly =
    let
        (.::.) t0 t1 = if h then T.concat [t0," :: ",t1] else T.concat[t0," : ",t1]
        snTxt = T.pack sn

        generateGetter (et,n,_) =
            let
                nTxt = T.pack n
                fnName = T.concat["get",capitalize nTxt]
                newEdts = map (\(et,nn,dd) -> if nn == n then (et,nn,dd) else (et,"_",dd)) edts 
            in T.unlines
                [
                    T.concat [fnName .::. if getOnly then "Model" else snTxt," -> ",et2Txt h False et]
                ,   T.concat [fnName," ",generatePattern (if getOnly then "Static.Types."++sn++"."++sn else sn,newEdts)," = ",nTxt]
                ]
        generateSetter (et,n,_) =
            let
                nTxt = T.pack n
                fnName = T.concat["update",capitalize nTxt]
                newValue = map (\(ett,nn,dd) -> if nn == n then (ett,"new"++nn,dd) else (ett,nn,dd)) edts
            in T.unlines
                [
                    T.concat [fnName .::. et2Txt h False et, " -> ",snTxt, " -> ",snTxt]
                ,   T.concat [fnName," new",nTxt," ",generatePattern (sn,edts)," = ", generatePattern (sn,newValue)]
                ]
        generateUpdater (et,n,_) =
            let
                nTxt = T.pack n
                fnName = T.concat["alter",capitalize nTxt]
                newValue = map (\(ett,nn,dd) -> if nn == n then (ett,"new"++nn,dd) else (ett,nn,dd)) edts
            in T.unlines
                [
                    T.concat [fnName .::. "(",et2Txt h False et," -> ",et2Txt h False et,") -> ",snTxt," -> ",snTxt]
                ,   T.concat [fnName," f ",generatePattern (sn,edts)," = "]
                ,   T.concat ["    let"]
                ,   T.concat ["        new",nTxt," = f ",nTxt]
                ,   T.concat ["    in"]
                ,   T.concat ["        ",generatePattern (sn,newValue)]
                ]

    in
        T.unlines 
            ([
                if h then T.concat ["module Static.Helpers.",snTxt," where\n\nimport Data.Map as Dict"]
                else      T.concat ["module Static.Helpers.",snTxt,if getOnly then "Model" else ""," exposing (..)\nimport Dict exposing (Dict)"]
            ,   if getOnly then 
                    T.concat ["import Static.Types.",snTxt," exposing(Model(..))"] 
                else ""
            ,   T.concat ["import Static.Types",if not h then " exposing(..)\nimport Static.ExtraUserTypes exposing(..)" else ""]
            ,   T.unlines $ map generateGetter edts
            ] ++
                if not getOnly then  
                [
                    T.unlines $ map generateSetter edts
                ,   T.unlines $ map generateUpdater edts
                ] else [])

capitalize :: T.Text -> T.Text
capitalize txt =
    case T.unpack txt of
        h:rest -> T.pack $ toUpper h : rest
        _ -> txt