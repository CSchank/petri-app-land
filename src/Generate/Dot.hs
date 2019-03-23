{-# LANGUAGE OverloadedStrings #-}

module Generate.Dot where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import qualified Data.Maybe as MA
import Types
import Generate.Types
import                  System.FilePath.Posix   ((</>),(<.>))
import Utils
import                  System.Directory


generateDot :: ClientServerApp -> FilePath -> Bool -> IO ()
generateDot (startCp
            ,netLst
            ,cExtraTlst
            ) fp renderTypes = 
    let
        
    in do
        createDirectoryIfMissing True (fp </> "Diagrams")
        mapM_ (\net -> writeIfNew 0 (fp </> "Diagrams" </> (T.unpack $ getNetName net) <.> "dot") (generateNetDot net)) netLst
    

generateNetDot :: Net -> T.Text
generateNetDot
        (Net name startingPlace places transitions plugins)
        =
    let
        nodes :: T.Text
        nodes =
            let
                placeNodes =
                    map (\place -> T.concat["  ",getPlaceName place,"node [label=\"",getPlaceName place,"\"]"]) places
                transitionNodes =
                    map (\trans -> T.concat["  ",getTransitionName trans,"node [label=\"",getTransitionName trans,"\",shape=box]"]) transitions
            in
                T.unlines $
                    placeNodes ++ transitionNodes

        transitionsTxt :: T.Text
        transitionsTxt =
            let
                oneConnection :: T.Text -> (T.Text, Maybe (T.Text, Constructor)) -> T.Text
                oneConnection transName (from,mTo) =
                    case mTo of
                        Just (to, (msgName,edts)) ->
                            let
                                sameTailName = 
                                    T.concat[from,T.pack msgName,to]
                            in
                            T.unlines
                                [
                                    T.concat["  ",transName,"node -> ",from,"node [arrowhead=none",",sametail=",sameTailName,"]"]
                                ,   T.concat["  ",transName,"node -> ",to,"node [label=\"",T.pack msgName,"\"",",sametail=",sameTailName,"]"]
                                ]
                        Nothing ->
                            let
                                sameTailName = 
                                    T.concat[from,"same"]
                            in   
                            T.unlines
                            [
                                T.concat["  ",transName,"node -> ",from,"node [arrowhead=none",",sametail=",sameTailName,",style=dashed]"]
                            ,   T.concat["  ",transName,"node -> ",from,"node [","sametail=",sameTailName,",","style=dashed]"]
                            ]

                oneTrans :: NetTransition -> T.Text
                oneTrans (NetTransition _ (transName,_) connections cmd) =
                    T.unlines $ map (oneConnection $ T.pack transName) connections
                oneTrans (ClientTransition {}) =
                    T.unlines $ ["Not yet supported."]
                oneTrans (CmdTransition {}) =
                    T.unlines $ ["Not yet supported."]

                allTransitions =
                    T.unlines $ map oneTrans transitions
            in
                allTransitions
    in
    T.unlines 
    [
        "digraph D {"
    ,   nodes
    ,   ""
    ,   transitionsTxt
    ,   "}"
    ]