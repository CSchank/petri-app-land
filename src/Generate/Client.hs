{-# LANGUAGE OverloadedStrings #-}

module Generate.Client where

import                  Control.Monad (unless)
import                  Generate.Codec
import                  Generate.Types
import                  Generate.Standalone
import qualified        Data.Map                as M
import qualified        Data.Set                as S
import qualified        Data.Text               as T
import qualified        Data.Text.IO            as TIO
import qualified        Data.Char               as Char
import                  Types
import                  Utils
import                  Generate.Net.Client
import                  Generate.Plugins
import                  System.Directory
import                  System.FilePath.Posix   ((</>),(<.>))
import                  Data.Maybe              (mapMaybe,fromMaybe,fromJust)
import                  Data.Time               (getCurrentTime)
import Data.Foldable (find)
import TypeHelpers

generateClient :: Bool -> Bool -> FilePath -> ClientServerApp -> IO ()
generateClient gsvg onlyStatic fp 
                  (startNet
                  ,netLst
                  ,cExtraTlst
                  ,sExtraTlst
                  ) =
    let
        cExtraT = M.fromList $ map (\(ElmCustom n constrs) -> (n,ElmCustom n constrs)) cExtraTlst
        sExtraT = M.fromList $ map (\(ElmCustom n constrs) -> (n,ElmCustom n constrs)) sExtraTlst
        netNames = map (\(HybridNet name _ _ _ _) -> name) netLst
        init :: T.Text
        init = 
            let
                {-startNet :: Net
                startNet = fromJust $ find (\(HybridNet name _ _ _ _) -> name == startNet) netLst
                startPlace (HybridNet _ startPlace places _ _) = find (\(HybridPlace name _ _ _ _ _ _ _) -> name == startPlace) places
                mCmd = 
                    case startPlace startNet of
                        (HybridPlace _ _ _ _ _ _ (Just cmd,_) _) -> -}
                --decl = case mCmd of
                --    Just _  -> T.concat [fnName, " = (", constr2Def extraTypes (T.unpack name,serverPlaceState),", Cmd.none)"]
                 --   Nothing -> T.concat [fnName, " = ", constr2Def extraTypes (T.unpack name,serverPlaceState)]

            in
            T.unlines
            [
                "module Static.Init exposing (..)"
            ,   "import Static.Types"
            ,   T.unlines $ map (\n -> T.concat ["import ",n,".Static.Init exposing(..)"]) netNames
            ,   ""
            ,   T.concat["init = ",startNet,".Static.Init.init"]
            ,   "-- reference to the initial Net"
            ,   T.concat["initNet :: NetModel"]
            ,   T.concat["initNet = ",startNet]
            ]
        types :: T.Text
        types = 
            let
                netUnion    = ec "NetModel" $ map (\nname -> constructor (T.unpack nname) []) netNames
                netMsgUnion = ec "NetTransition" $ map (\nname -> constructor (T.unpack nname ++ "Trans") [edt (ElmType $ T.unpack nname ++ ".Static.Types.Transition") "" ""]) netNames
                netOutgoingMsgUnion = ec "NetOutgoingMessage" $ map (\nname -> constructor (T.unpack nname ++ "OMsg") [edt (ElmType $ T.unpack nname ++ ".Static.Types.ClientMessage") "" ""]) netNames
            in
            T.unlines 
            [
                "module Static.Types where"
            ,   T.unlines $ map (\n -> T.concat ["import ",n,".Static.Types"]) netNames
            ,   ""
            ,   "-- a type identifying all of the nets in the server"
            ,   generateType Elm False [DShow,DOrd,DEq] netUnion
            ,   "-- a union type of all the nets and their transitions"
            ,   generateType Elm False [DShow,DOrd,DEq] netMsgUnion
            ,   "-- a union type of all the nets and their transitions"
            ,   generateType Elm False [DShow,DOrd,DEq] netOutgoingMsgUnion
            ]
        decode :: T.Text
        decode = 
            let

            in
            T.unlines
            [
                "module Static.Decode exposing(..)"
            ,   "import Static.Types"
            ,   "import Utils.Utils"
            ,   T.unlines $ map (\n -> T.concat ["import ",n,".Static.Decode"]) netNames
            ,   ""
            ,   "decodeIncomingMessage :: String -> NetModel -> Result T.Text NetTransition"
            ,   "decodeIncomingMessage txt clientNet ="
            ,   "    case clientNet of"
            ,   T.concat $ map (\netName -> T.concat["        ",netName," -> rMap ",netName,"Trans $ fst $ ",netName,".Static.Decode.decodeTransition (Err \"\",T.splitOn \"\\0\" txt)"]) netNames
            ]
        update :: T.Text
        update = 
            let
                updateCase netName = 
                    T.unlines 
                    [
                        T.concat["        ",netName,"Trans msg ->"]
                    ,   "            let"
                    ,   T.concat["                (newNetState, clientMessages, mCmd) = ",netName,".update tld mClientID msg (fromJust $ TM.lookup $ serverState state)"]
                    ,   T.concat["                cmd = fmap (\\m -> cmdMap ",netName,"Trans m) mCmd"]
                    ,   T.concat["                cMsgs = map (\\(cId,m) -> (cId,",netName,"OMsg m)) clientMessages"]
                    ,   T.concat["                newServerState = state { serverState = TM.insert newNetState (serverState state) }"]
                    ,   "            in (newServerState, cMsgs, cmd)"
                    ]
                disconnectCase netName = T.concat ["                ",netName," {} -> ",netName,".disconnect tld clientID (fromJust $ TM.lookup $ serverState state)"]
            in
            T.unlines
            [
                "module Static.Update exposing(..)"
            ,   T.unlines $ map (\n -> T.concat ["import ",n,".Static.Update as ",n]) netNames
            ,   "import Static.Types"
            ,   "import Utils.Utils"
            ,   ""
            ,   "update :: TopLevelData -> NetTransition -> ClientState -> (ClientState, Maybe (Cmd NetTransition))"
            ,   "update tld mClientID netTrans state ="
            ,   "    case netTrans of"
            ,   T.unlines $ map updateCase netNames
            ]
        encode :: T.Text
        encode = 
            let
                encodeCase netName = T.concat["        ",netName,"OMsg msg -> ",netName,".encodeClientMessage msg"]
            in
            T.unlines
            [
                "module Static.Encode exposing(..)"
            ,   T.unlines $ map (\n -> T.concat ["import ",n,".Static.Encode as ",n]) netNames
            ,   "import Static.ServerTypes"
            ,   "import Static.Types"
            ,   ""
            ,   "encodeOutgoingMessage :: NetOutgoingMessage -> String"
            ,   "encodeOutgoingMessage netTrans ="
            ,   "    case netTrans of"
            ,   T.unlines $ map encodeCase netNames
            ]

    in do
        createDirectoryIfMissing True (fp </> "client" </> "src" </> "Static")
        copyDirectory "ClientTemplate/" (fp </> "client/")
        writeIfNew 0 (fp </> "client" </> "src" </> "Static" </> "Init" <.> "hs") init 
        writeIfNew 0 (fp </> "client" </> "src" </> "Static" </> "Types" <.> "hs") types 
        writeIfNew 0 (fp </> "client" </> "src" </> "Static" </> "Decode" <.> "hs") decode 
        writeIfNew 0 (fp </> "client" </> "src" </> "Static" </> "Encode" <.> "hs") encode 
        writeIfNew 0 (fp </> "client" </> "src" </> "Static" </> "Update" <.> "hs") update 
        mapM_ (Generate.Net.Client.generate sExtraT fp) netLst