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
        disclaimer date = T.unlines ["{-"
                                ,T.concat["    THIS FILE WAS AUTOMATICALLY GENERATED AT ", T.pack $ show date,"."]
                                , "    IMPORTANT: USE THIS FILE FOR REFERENCE ONLY. YOU SHOULD NOT MODIFY THIS FILE. INSTEAD, MODIFY THE STATE DIAGRAM AND REGENERATE THIS FILE."
                                , "    MODIFYING ANY FILES INSIDE THE static DIRECTORY COULD LEAD TO UNEXPECTED ERRORS IN YOUR APP."
                                ,"-}"
                                ]
        serverStateTypeTxt =
            generateType True True $ ElmCustom "Model" $ sStates
        serverMsgs = S.toList $ serverTransFromClient `S.union` serverTransitions
        serverMsgType = ElmCustom "Message" serverMsgs
        serverMsgTypeTxt = generateType True True $ serverMsgType
        serverTransFromClient = S.fromList $ mapMaybe (\(_,(_,trans)) -> trans) $ M.toList cDiagram
        serverTransitions = S.fromList $ map (\((_,trans),_) -> trans) $ M.toList sDiagram
        inputTypesTxt = T.unlines $ map (\(name,constrs) -> generateType True True $ ElmCustom ("I"++name) [("I"++name,constrs)]) serverMsgs
        returnTypesTxt = T.unlines $ map (\(name,constrs) -> generateType True True $ ElmCustom ("R"++name) [("R"++name,constrs)]) sStates
        typesHs = [
                 "module Static.Types where\n"
                ,"-- Server States"
                , serverStateTypeTxt, ""
                ,"-- Server Messages"
                ,serverMsgTypeTxt,""
                ,"-- Input Types"
                ,inputTypesTxt
                ,"-- Return Types"
                ,returnTypesTxt
                ]

        encoder = generateEncoder True serverMsgType
        encoderHs = [encoder]

        decoder = generateDecoder True serverMsgType
        decoderHs = ["module Static.Decode where\n"
                    ,"import Utils.Decode"
                    ,"import Static.Types\n"
                    ,decoder]



    in do
        createDirectoryIfMissing True $ fp </> "app"
        createDirectoryIfMissing True $ fp </> "src" </> "utils"
        createDirectoryIfMissing True $ fp </> "src" </> "static"
        createDirectoryIfMissing True $ fp </> "src" </> "userApp"
        currentTime <- getCurrentTime
        TIO.writeFile (fp </> "src" </> "static" </> "Types" <.> "hs") $ T.unlines $ disclaimer currentTime : typesHs
        TIO.writeFile (fp </> "src" </> "utils" </> "Decode" <.> "hs") $ T.unlines $ disclaimer currentTime : [decodeHs]
        TIO.writeFile (fp </> "src" </> "static" </> "Encode" <.> "hs")      $ T.unlines $ disclaimer currentTime : encoderHs
        TIO.writeFile (fp </> "src" </> "static" </> "Decode" <.> "hs")      $ T.unlines $ disclaimer currentTime : decoderHs
        putStrLn $ show serverTransitions