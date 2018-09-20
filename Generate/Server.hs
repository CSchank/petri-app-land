{-# LANGUAGE OverloadedStrings #-}

module Generate.Server where

import                  Generate.Codec
import                  Generate.Types
import qualified        Data.Map                as M
import qualified        Data.Set                as S
import qualified        Data.Text               as T
import qualified        Data.Text.IO            as TIO
import                  Types
import                  Generate.Types
import                  System.Directory
import                  System.FilePath.Posix   ((</>),(<.>))
import                  Data.Maybe              (mapMaybe)
import                  Data.Time               (getCurrentTime)
import                  ServerTemplate.Decode

generateServer :: FilePath -> ClientServerApp -> IO ()
generateServer fp (startCs,startSs,cStates,sStates,cDiagram,sDiagram) = 
    let
        types date = T.unlines ["{-"
                                ,T.concat["    THIS FILE WAS AUTOMATICALLY GENERATED AT ", T.pack $ show date,"."]
                                , "    IMPORTANT: USE THIS FILE FOR REFERENCE ONLY. YOU SHOULD NOT MODIFY THIS FILE. INSTEAD, MODIFY THE STATE DIAGRAM AND REGENERATE THIS FILE."
                                , "    MODIFYING ANY FILES INSIDE THE static DIRECTORY COULD LEAD TO UNEXPECTED ERRORS IN YOUR APP."
                                ,"-}"
                                ,"-- Server States"
                                , serverStateType, ""
                                ,"-- Server Messages"
                                ,serverMsgType,""
                                ,"-- Input Types"
                                ,inputTypes
                                ,"-- Return Types"
                                ,returnTypes
                                ]
        serverStateType =
            generateType True True $ ElmCustom "Model" $ sStates

        serverMsgs = S.toList $ serverTransFromClient `S.union` serverTransitions
        serverMsgType = generateType True True $ ElmCustom "Message" serverMsgs
        serverTransFromClient = S.fromList $ mapMaybe (\(_,(_,trans)) -> trans) $ M.toList cDiagram
        serverTransitions = S.fromList $ map (\((_,trans),_) -> trans) $ M.toList sDiagram

        inputTypes = T.unlines $ map (\(name,constrs) -> generateType True True $ ElmCustom ("I"++name) [("I"++name,constrs)]) serverMsgs
        returnTypes = T.unlines $ map (\(name,constrs) -> generateType True True $ ElmCustom ("R"++name) [("R"++name,constrs)]) sStates


    in do
        createDirectoryIfMissing True $ fp </> "app"
        createDirectoryIfMissing True $ fp </> "src" </> "utils"
        createDirectoryIfMissing True $ fp </> "src" </> "static"
        createDirectoryIfMissing True $ fp </> "src" </> "userApp"
        currentTime <- getCurrentTime
        TIO.writeFile (fp </> "src" </> "static" </> "Types" <.> "hs") $ (types currentTime)
        TIO.writeFile (fp </> "src" </> "utils" </> "Decode" <.> "hs") decodeHs
        putStrLn $ show serverTransitions