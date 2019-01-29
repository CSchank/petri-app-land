{-# LANGUAGE OverloadedStrings #-}

module Generate.Standalone where

import ClientServerSpec
import Types
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as S
import qualified Data.Map as M
import                  System.FilePath.Posix   ((</>),(<.>))
import Generate.Types

generateStandalones :: ExtraTypes -> FilePath -> [ClientState] -> IO ()
generateStandalones ects fp cStates =
    let
        cState2ConstrMap (ClientState (n,edt)) = (n,edt)
        cState2ConstrMap (ClientStateWithSubs (n,edt) _) = (n,edt)
        cStatesList = S.toList $ S.fromList $ map cState2ConstrMap cStates
        ecMap = M.fromList $ map (\(ElmCustom n constrs) -> (n,ElmCustom n constrs)) ects
    in
        mapM_ (\(sn,edt) -> TIO.writeFile (fp </> "client" </> "src" </> "Static" </> "Standalone" </> sn <.> "elm") $ generateStandalone ecMap (sn,edt)) cStatesList

generateStandalone :: M.Map String ElmCustom -> Constructor -> T.Text
generateStandalone ecMap (sn,edts) =
    let
        snTxt = T.pack sn
    in
        T.unlines 
            [
                T.concat ["module Standalone.",snTxt," exposing(..)"]
            ,   "import GraphicSVG exposing(..)"
            ,   T.concat ["import View.",snTxt]
            ,   T.concat ["import Static.Types.",snTxt," exposing (Model(..),Msg)"]
            ,   T.concat ["import Utils.Utils exposing (error)"]
            ,   T.concat ["import Static.ExtraUserTypes exposing(..)"]
            ,   T.concat ["import Dict exposing (Dict)"]
            ,   T.concat ["import Json.Decode as D"],""
            ,   "main : EllieApp D.Value Model Msg"
            ,   T.concat ["main = ellieApp { init = \\_ -> (model, Cmd.none), view = \\m -> { body = View.",snTxt,".view m, title = View.",snTxt,".title m }, update = \\_ m -> (m,Cmd.none), subscriptions = \\_ -> Sub.none }"],""
            ,   "--Change the model here to preview your state"
            ,   T.concat["model : Model"]
            ,   T.concat["model = ",constr2Def ecMap (sn,edts)]
            ]