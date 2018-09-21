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
generateServer fp (startCs
                  ,startSs
                  ,cStates
                  ,sStates
                  ,cExtraT
                  ,sExtraT
                  ,cDiagram
                  ,sDiagram) = 
    let
        disclaimer date = T.unlines ["{-"
                                ,T.concat["    THIS FILE WAS AUTOMATICALLY GENERATED AT ", T.pack $ show date,"."]
                                , "    IMPORTANT: USE THIS FILE FOR REFERENCE ONLY. YOU SHOULD NOT MODIFY THIS FILE. INSTEAD, MODIFY THE STATE DIAGRAM AND REGENERATE THIS FILE."
                                , "    MODIFYING ANY FILES INSIDE THE static DIRECTORY COULD LEAD TO UNEXPECTED ERRORS IN YOUR APP."
                                ,"-}"
                                ]
        serverStateTypeTxt = 
            generateType True True $ ElmCustom "Model" $ M.elems sStates
        serverMsgs = S.toList $ serverTransFromClient `S.union` serverTransitions
        serverMsgType = ElmCustom "IncomingMessage" serverMsgs
        serverMsgTypeTxt = generateType True True $ serverMsgType

        serverTransFromClient = S.fromList $ mapMaybe (\(_,(_,trans)) -> trans) $ M.toList cDiagram
        serverTransitions = S.fromList $ map (\((_,trans),_) -> trans) $ M.toList sDiagram

        clientMsgType = ElmCustom "OutgoingMessage" clientMsgs
        clientMsgTypeTxt = generateType True True $ clientMsgType
        clientMsgs = S.toList $ S.fromList $ map (\((_,trans),(_,_)) -> trans) $ M.toList cDiagram

        inputTypesTxt = T.unlines $ map (\(name,constrs) -> generateType True True $ ElmCustom ("I"++name) [("I"++name,constrs)]) serverMsgs
        returnTypesTxt = T.unlines $ map (\(name,constrs) -> generateType True True $ ElmCustom ("R"++name) [("R"++name,constrs)]) $ M.elems sStates
        returnMsgTypesTxt = T.unlines $ map (\(name,constrs) -> generateType True True $ ElmCustom ("M"++name) [("M"++name,constrs)]) clientMsgs

        extraTypes    = map snd $ M.toList sExtraT
        extraTypesTxt = T.unlines $ map (generateType True True) $ extraTypes


        typesHs = 
                [
                "{-"
                ,"    Looking for a place to define your own algebraic data types for internal use? Look at you becoming a real Haskell programmer! What a great idea."
                ,"    However, instead of defining them here, why not define them in userApp/Types.hs instead? We've conveniently created it for you :)"
                ,"-}\n"
                ,"module Static.Types where\n"
                ,"-- Server States"
                , serverStateTypeTxt, ""
                ,"-- Incoming Server Messages"
                ,serverMsgTypeTxt,""
                ,"-- Outgoing Client Messages"
                ,clientMsgTypeTxt,""
                ,"-- Extra internal types"
                ,extraTypesTxt,""
                ,"-- Input Types"
                ,inputTypesTxt
                ,"-- Return Types"
                ,returnTypesTxt
                ,"-- Return Messages Types"
                ,returnMsgTypesTxt
                ]

        encoder = generateEncoder True serverMsgType
        extraTypesEncoder = T.unlines $ map (generateEncoder True) extraTypes
        encoderHs = [encoder
                    ,extraTypesEncoder]

        decoder = generateDecoder True serverMsgType
        extraTypesDecoder = T.unlines $ map (generateDecoder True) extraTypes
        decoderHs = ["module Static.Decode where\n"
                    ,"import Utils.Decode"
                    ,"import Static.Types\n"
                    ,decoder
                    ,extraTypesDecoder]

        createUserUpdate :: ((String, ServerTransition), (String, Maybe ClientTransition)) -> T.Text
        createUserUpdate ((s0,(tn,tetd)),(s1,mCt)) =
            let
                s0Cons = case (M.lookup s0 sStates) of
                            Just constr -> constr
                            Nothing -> error $ "State" ++ s0 ++ " does not exist in the map."
                s1Cons = case (M.lookup s1 sStates) of
                            Just constr -> constr
                            Nothing -> error $ "State" ++ s0 ++ " does not exist in the map."
                name = T.concat ["update", T.pack s0, T.pack tn, T.pack s1]
                typE = T.concat [name, " :: ", "M", T.pack tn, " -> Model -> R",T.pack s1]
                decl = T.concat [name, " ", generatePattern ("M"++tn,tetd), generatePattern s0Cons, " = error \"Update function ",name," not defined. Please define it in userApp/Update.hs.\""]
            in
                T.unlines
                    [
                        "--Hint: replace the error with the return value of this update function."
                    ,   typE
                    ,   decl
                    ,   ""
                    ]

        userUpdateHs = [
                        "module Update where"
                       ,"import Static.Types --types are generated from the state diagram, don't remove this import"
                       ,""
                       ,"{-"
                       ,"    Fill in the update functions to control the behaviour of your server. Don't change the type declarations or the input definitions"
                       ,"    of the functions. Types have been tagged with M and R for Message and Return types, respectively. These represent constructors"
                       ,"    identical to your messages and states and arguments are unwrapped in the background. This ensures your code follows the state"
                       ,"    diagram you created. If you would like to change these input and return types, edit the state diagram and regenerate the code."
                       ,""
                       ,"    The functions are named as follows: updateCurrentMessageNext where"
                       ,"         - Current is the current server state"
                       ,"         - Message is the message being received"
                       ,"         - Next is the next server state"
                       ,"-}"
                       ,T.unlines $ map createUserUpdate $ M.toList sDiagram
                       ]
        userTypesHs = ["{-"
                      ,"    Define your own types for internal use here. Note that any types used in the state or messages should be defined in the state"
                      ,"    diagram and generated for use in your program. These types can be used within update / view functions only."
                      ,"-}"
                      ]

        userInitHs = ["module Init where"
                     ,"import Static.Types\n"
                     ,"{-"
                     ,"    Define the starting point of your model here. Note: the starting state is defined within your state diagram. Only the starting"
                     ,"    state's data can be modified here. If you'd like a completely different starting state, modify your state diagram and regenerate"
                     ,"    the code. Note that the return type of the function is tagged with \"R\". This is a type identical to your initial state, with"
                     ,"    the same arguments. This \"R\"-type will be unwrapped for you in the background. This ensures consistency with your state diagram."
                     ,"-}\n"
                     ,"-- Hint: replace error with the return value of your initial state"
                     ,T.concat["init :: R",T.pack $ startSs]
                     ,"init = error \"Initial server state not defined. Please define it in userApp/Init.hs.\""
                     ]

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
        TIO.writeFile (fp </> "src" </> "userApp" </> "Update" <.> "hs")      $ T.unlines $ userUpdateHs
        TIO.writeFile (fp </> "src" </> "userApp" </> "Types" <.> "hs")      $ T.unlines $ userTypesHs
        TIO.writeFile (fp </> "src" </> "userApp" </> "Init" <.> "hs")      $ T.unlines $ userInitHs
        putStrLn $ show serverTransitions