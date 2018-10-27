{-# LANGUAGE OverloadedStrings #-}

module Generate.Dot where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import qualified Data.Maybe as MA
import Types
import Generate.Types

generateDot :: ClientServerApp -> IO ()
generateDot (startClient
                  ,startServer
                  ,clientStates
                  ,serverStates
                  ,cExtraT
                  ,sExtraT
                  ,cDiagram
                  ,sDiagram)= 
    let
        replace :: [T.Text] -> [T.Text]
        replace (l1:ls) = ( case lookup l1 swaps of
                            Just new -> new
                            Nothing  -> l1
                        )
                        : (replace ls)
        replace [] = []

        swaps = [("*CLIENTD", T.unlines (clientDiagram))
                ,("*SERVERD", T.unlines (serverDiagram))
                ,("*COMMD",   T.unlines (c2sDiagram ++ s2cDiagram))
                ,("*START",   T.unlines $ T.concat [ T.pack startClient,"[peripheries=2];"]:T.concat [ T.pack startServer,"[peripheries=2];"]:[])
                ]

        clientList = M.toList cDiagram
        serverList = M.toList sDiagram
        clientDiagram = map clientStateToGViz clientList
        serverDiagram = map serverStateToGViz serverList
        c2sDiagram = MA.mapMaybe (c2sStateToGViz $ T.pack startServer) clientList
        s2cDiagram = MA.mapMaybe (s2cStateToGViz $ T.pack startClient) serverList
    in do
        dotTemplate <- Tio.readFile "template.dot"
        Tio.writeFile ("rgbApp.dot") $ T.unlines $ replace $ T.lines dotTemplate
        putStrLn $ show clientDiagram

--clientStateToGViz :: ClientStateDiagram -> T.Text
clientStateToGViz ((cState0S, cTrans), (cState1S, mServe)) =
    let 
        cState0 = T.pack cState0S
        cState1 = T.pack cState1S
        fakeName = T.concat["fake", cState0, T.pack $ fst cTrans,cState1]
    in
        case mServe of 
            Just m -> T.unlines $ 
                        T.concat ["\t\t", cState0,"->",fakeName," [label=\"", generateConstructor False False cTrans,"\",arrowhead=none];"] :
                        T.concat ["\t\t",fakeName, " [shape=diamond, width=0.2, height=0.2, label=\"\"];"] :
                        T.concat ["\t\t",fakeName,"->",cState1,";"] : []
            Nothing -> T.concat ["\t\t", cState0,"->", cState1," [label=\"",generateConstructor False False cTrans,"\"];"]

serverStateToGViz ((sState0S, sTrans), (sState1S, mClien)) =
    let 
        sState0 = T.pack sState0S
        sState1 = T.pack sState1S
        fakeName = T.concat["fake", sState0, T.pack $ fst sTrans,sState1]
    in
        case mClien of
            NoClientMessage    -> T.concat ["\t\t", sState0,"->", sState1," [label=\"",generateConstructor False False sTrans,"\"];"]
            _ -> T.unlines $ 
                    T.concat ["\t\t", sState0,"->",fakeName," [label=\"",generateConstructor False False sTrans,"\",arrowhead=none];"] :
                    T.concat ["\t\t",fakeName, " [shape=diamond, width=0.2, height=0.2, label=\"\"];"] :
                    T.concat ["\t\t",fakeName,"->", sState1,";"] : []
        
c2sStateToGViz firstServe ((cState0S, cTrans), (cState1S, Just mClien)) = 
    let 
        cState0 = T.pack cState0S
        cState1 = T.pack cState1S
        fakeName = T.concat["fake", cState0, T.pack $ fst cTrans,cState1]
    in
        Just $ T.concat ["\t",fakeName,"->", firstServe," [label=\"",generateConstructor False False mClien,"\",lhead=cluster_1,style=dashed];"]
c2sStateToGViz _ ((_, _), (_, Nothing)) = Nothing


s2cStateToGViz firstClien ((sState0S, sTrans), (sState1S, ToAll cMsg)) = 
    let 
        sState0 = T.pack sState0S
        sState1 = T.pack sState1S
        fakeName = T.concat["fake", sState0, T.pack $ fst sTrans,sState1]
    in
        Just $ T.concat ["\t",fakeName,"->", firstClien, "[label=\"∀ ",generateConstructor False False cMsg,"\",lhead=cluster_0,style=dashed];"]
s2cStateToGViz firstClien ((sState0S, sTrans), (sState1S, ToSender cMsg)) = 
    let 
        sState0 = T.pack sState0S
        sState1 = T.pack sState1S
        fakeName = T.concat["fake", sState0, T.pack $ fst sTrans,sState1]
    in
        Just $ T.concat ["\t",fakeName,"->", firstClien, "[label=\"✉ ",generateConstructor False False cMsg,"\",lhead=cluster_0,style=dashed];"]
s2cStateToGViz firstClien ((sState0S, sTrans), (sState1S, ToSenderAnd cMsg)) = 
    let 
        sState0 = T.pack sState0S
        sState1 = T.pack sState1S
        fakeName = T.concat["fake", sState0, T.pack $ fst sTrans,sState1]
    in
        Just $ T.concat ["\t",fakeName,"->", firstClien, "[label=\"✉+ ",generateConstructor False False cMsg,"\",lhead=cluster_0,style=dashed];"]
s2cStateToGViz _ ((_, _), (_, NoClientMessage)) = Nothing