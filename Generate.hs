{-# LANGUAGE OverloadedStrings #-}

module Generate where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import qualified Data.Maybe as MA
import Types

generateClientState :: ClientServerApp -> [String]
generateClientState (_,_,clientState, _) =
    let 
        states = S.toList $ S.fromList $ map (fst . fst) $ M.toList clientState
    in
        map show states

generateDot :: ClientServerApp -> IO ()
generateDot (startClient,startServer,clientState, serverState) = 
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
                ,("*START",   T.unlines $ T.concat [s2GViz startClient,"[peripheries=2];"]:T.concat [s2GViz startServer,"[peripheries=2];"]:[])
                ]

        clientList = M.toList clientState
        serverList = M.toList serverState
        clientDiagram = map clientStateToGViz $ clientList
        serverDiagram = map serverStateToGViz $ serverList
        c2sDiagram = MA.mapMaybe (c2sStateToGViz $ fst $ fst $ serverList !! 0) $ clientList
        s2cDiagram = MA.mapMaybe (s2cStateToGViz $ fst $ fst $ clientList !! 0) $ serverList
    in do
        dotTemplate <- Tio.readFile "template.dot"
        Tio.writeFile ("rgbApp.dot") $ T.unlines $ replace $ T.lines dotTemplate
        putStrLn $ show clientDiagram


s2GViz (PlainType name) = T.pack name
s2GViz (IntType name) = T.concat ["\"",T.pack name," Int\""]
s2GViz (DoubleType name) = T.concat ["\"",T.pack name," Float\""]

--clientStateToGViz :: ClientStateDiagram -> T.Text
clientStateToGViz ((cState0, cTrans), (cState1, mServe)) =
    let 
        fakeName = T.concat["fake", s2GViz cState0, s2GViz cState1]
    in
        case mServe of 
            Just m -> T.unlines $ 
                        T.concat ["\t\t",s2GViz cState0,"->",fakeName," [label=\"",s2GViz cTrans,"\",arrowhead=none];"] :
                        T.concat ["\t\t",fakeName, " [shape=point, width=0, height=0];"] :
                        T.concat ["\t\t",fakeName,"->",s2GViz cState1,";"] : []
            Nothing -> T.concat ["\t\t",s2GViz cState0,"->",s2GViz cState1," [label=\"",s2GViz cTrans,"\"];"]

serverStateToGViz ((sState0, sTrans), (sState1, mClien)) =
    let 
        s2GViz (PlainType name) = T.pack name
        s2GViz (IntType name) = T.concat ["\"",T.pack name," Int\""]
        s2GViz (DoubleType name) = T.concat ["\"",T.pack name," Double\""]
        fakeName = T.concat["fake", s2GViz sState0, s2GViz sState1]
    in
        case mClien of
            Just m -> T.unlines $ 
                        T.concat ["\t\t",s2GViz sState0,"->",fakeName," [label=\"",s2GViz sTrans,"\",arrowhead=none];"] :
                        T.concat ["\t\t",fakeName, " [shape=point, width=0, height=0];"] :
                        T.concat ["\t\t",fakeName,"->",s2GViz sState1,";"] : []
            Nothing -> T.concat ["\t\t",s2GViz sState0,"->",s2GViz sState1," [label=\"",s2GViz sTrans,"\"];"]

c2sStateToGViz firstServe ((cState0, cTrans), (cState1, Just mClien)) = 
    let 
        s2GViz (PlainType name) = T.pack name
        s2GViz (IntType name) = T.concat ["\"",T.pack name," Int\""]
        s2GViz (DoubleType name) = T.concat ["\"",T.pack name," Float\""]
        fakeName = T.concat["fake", s2GViz cState0, s2GViz cState1]
    in
        Just $ T.concat ["\t",fakeName,"->", s2GViz firstServe," [label=\"",s2GViz mClien,"\",lhead=cluster_1,style=dashed];"]
c2sStateToGViz _ ((_, _), (_, Nothing)) = Nothing


s2cStateToGViz firstClien ((sState0, sTrans), (sState1, Just mServe)) = 
    let 
        s2GViz (PlainType name) = T.pack name
        s2GViz (IntType name) = T.concat ["\"",T.pack name," Int\""]
        s2GViz (DoubleType name) = T.concat ["\"",T.pack name," Float\""]
        fakeName = T.concat["fake", s2GViz sState0, s2GViz sState1]
    in
        Just $ T.concat ["\t",fakeName,"->", s2GViz firstClien, "[label=\"",s2GViz mServe,"\",lhead=cluster_0,style=dashed];"]
s2cStateToGViz _ ((_, _), (_, Nothing)) = Nothing

generateCode :: ClientServerApp -> IO ()
generateCode scApp = do
    putStrLn "Hello"