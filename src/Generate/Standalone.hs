{-# LANGUAGE OverloadedStrings #-}

module Generate.Standalone where

import Types
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as S
import qualified Data.Map as M
import                  System.FilePath.Posix   ((</>),(<.>))
import Generate.Types

generateStandalones :: M.Map String ElmCustom -> T.Text -> FilePath -> [HybridPlace] -> IO ()
generateStandalones ecMap netName fp places =
    let
        cState2ConstrMap (HybridPlace name _ _ edts _ _ _) = (T.unpack name, edts)
        placeList = S.toList $ S.fromList $ map cState2ConstrMap places
    in
        mapM_ (\(pn,edt) -> TIO.writeFile (fp </> "client" </> "src" </> T.unpack netName </> "Static" </> "Standalone" </> pn <.> "elm") $ 
            generateStandalone ecMap netName (pn,edt)) placeList

generateStandalone :: M.Map String ElmCustom -> T.Text -> Constructor -> T.Text
generateStandalone ecMap netName (sn,edts) =
    let
        snTxt = T.pack sn
    in
        T.unlines 
            [
                T.concat ["module ",netName,".Static.Standalone.",snTxt," exposing(..)"]
            ,   "import Html exposing(..)"
            ,   "import Browser exposing(..)"
            ,   T.concat ["import ",netName,".View.",snTxt," as View"]
            ,   T.concat ["import ",netName,".Static.Types exposing (..)"]
            ,   T.concat ["import ",netName,".Static.ExtraTypes exposing (..)"]
            ,   T.concat ["import ",netName,".Static.Types.",snTxt," exposing (Msg)"]
            ,   T.concat ["import Dict exposing (Dict)"]
            ,   T.concat ["import Json.Decode as D"],""
            ,   T.concat ["main : Program () ",snTxt," Msg"]
            ,   T.concat ["main = Browser.document { init = \\_ -> (model, Cmd.none), view = \\m -> { body = [View.view m], title = View.title m }, update = \\_ m -> (m,Cmd.none), subscriptions = \\_ -> Sub.none }"],""
            ,   "--Change the model here to preview your state"
            ,   T.concat["model : ",snTxt]
            ,   T.concat["model = ",constr2Def ecMap (sn,edts)]
            ]