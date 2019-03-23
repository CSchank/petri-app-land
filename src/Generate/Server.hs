{-# LANGUAGE OverloadedStrings #-}

module Generate.Server where

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
import                  Generate.Net.Server
import                  Generate.Net.Client
import                  Generate.Plugins
import                  System.Directory
import                  System.FilePath.Posix   ((</>),(<.>))
import                  Data.Maybe              (mapMaybe,fromMaybe,fromJust)
import                  Data.Time               (getCurrentTime)
import Data.Foldable (find)
import TypeHelpers

generateServer :: Bool -> FilePath -> FilePath -> ClientServerApp -> IO ()
generateServer gsvg rootDir fp 
                  (startNet
                  ,netLst
                  ,extraTlst
                  ) =
    let
        extraT = M.fromList $ map (\(ElmCustom n constrs) -> (n,ElmCustom n constrs)) extraTlst
        netNames = map (\(Net name _ _ _ _) -> name) netLst
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
                "module Static.Init where"
            ,   "import Static.Types"
            ,   "import qualified Data.TMap as TM"
            ,   "import Data.Maybe (fromJust)"
            ,   T.unlines $ map (\n -> T.concat ["import ",n,".Static.Init"]) netNames
            ,   ""
            ,   T.concat["init = ",startNet,".Static.Init.init"]
            ,   T.concat["teardown = ",startNet,".Static.Init.teardown . fromJust . TM.lookup"]
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
            ,   generateType Haskell False [DShow,DOrd,DEq] netUnion
            ,   "-- a union type of all the nets and their transitions"
            ,   generateType Haskell False [DShow,DOrd,DEq] netMsgUnion
            ,   "-- a union type of all the nets and their transitions"
            ,   generateType Haskell False [DShow,DOrd,DEq] netOutgoingMsgUnion
            ]
        decode :: T.Text
        decode = 
            let

            in
            T.unlines
            [
                "{-# LANGUAGE OverloadedStrings #-}"
            ,   "module Static.Decode where"
            ,   "import Static.Types"
            ,   "import qualified Data.Text as T"
            ,   "import Utils.Utils"
            ,   "import Static.Result (Result(..))"
            ,   "import qualified Static.Result as Result"
            ,   T.unlines $ map (\n -> T.concat ["import ",n,".Static.Decode"]) netNames
            ,   ""
            ,   "decodeIncomingMessage :: T.Text -> NetModel -> Result T.Text NetTransition"
            ,   "decodeIncomingMessage txt clientNet ="
            ,   "    case clientNet of"
            ,   T.concat $ map (\netName -> T.concat["        ",netName," -> Result.map ",netName,"Trans $ fst $ ",netName,".Static.Decode.decodeTransition (Err \"\",T.splitOn \"\\0\" txt)"]) netNames
            ]
        update :: T.Text
        update = 
            let
                updateCase netName = 
                    T.unlines 
                    [
                        T.concat["        ",netName,"Trans msg ->"]
                    ,   "            let"
                    ,   T.concat["                (newNetState, clientMessages, mCmd) = ",netName,".update tld mClientID msg (safeFromJust \"update case\" $ TM.lookup $ serverState state)"]
                    ,   T.concat["                cmd = fmap (\\m -> Cmd.map ",netName,"Trans m) mCmd"]
                    ,   T.concat["                cMsgs = map (\\(cId,m) -> (cId,",netName,"OMsg m)) clientMessages"]
                    ,   T.concat["                newServerState = state { serverState = TM.insert newNetState (serverState state) }"]
                    ,   "            in (newServerState, cMsgs, cmd)"
                    ]
                disconnectCase netName = T.concat ["                ",netName," {} -> ",netName,".disconnect tld clientID (safeFromJust \"top level disconnect\" $ TM.lookup $ serverState state)"]
            in
            T.unlines
            [
                "module Static.Update where"
            ,   T.unlines $ map (\n -> T.concat ["import ",n,".Static.Update as ",n]) netNames
            ,   "import Static.Types"
            ,   "import qualified Data.TMap as TM"
            ,   "import Static.ServerTypes"
            ,   "import Utils.Utils"
            ,   "import Data.Maybe (fromJust,mapMaybe,isJust)"
            ,   "import qualified Data.IntMap.Strict as IM'"
            ,   "import qualified Static.Cmd as Cmd"
            ,   "import qualified Plugins.Users.Types as Users"
            ,   ""
            ,   "update :: TopLevelData -> Maybe ClientID -> NetTransition -> ServerState -> (ServerState, [(ClientID,NetOutgoingMessage)], Maybe (Cmd.Cmd NetTransition))"
            ,   "update tld mClientID netTrans state ="
            ,   "    case netTrans of"
            ,   T.unlines $ map updateCase netNames
            ,   "clientConnect :: TopLevelData -> ClientID -> ServerState -> ServerState"
            ,   "clientConnect tld clientID state ="
            ,   "    let"
            ,   T.concat["        newNetState = ",startNet,".clientConnect tld clientID (safeFromJust \"client connect\" $ TM.lookup $ serverState state)"]
            ,   "    in"
            ,   "        state { serverState = TM.insert newNetState $ serverState state }"
            ,   ""
            ,   "disconnect :: TopLevelData -> ClientID -> NetModel -> ServerState -> IO ServerState"
            ,   "disconnect tld clientID netModel state = do"
            ,   "        newNetState <-"
            ,   "            case netModel of"
            ,   T.unlines $ map disconnectCase netNames
            ,   "        return $ state"
            ,   "           { serverState = TM.insert newNetState $ serverState state"
            ,   "           , clients = IM'.delete clientID $ clients state"
            ,   "           }"
            ]
        encode :: T.Text
        encode = 
            let
                encodeCase netName = T.concat["        ",netName,"OMsg msg -> ",netName,".encodeClientMessage msg"]
            in
            T.unlines
            [
                "module Static.Encode where"
            ,   T.unlines $ map (\n -> T.concat ["import ",n,".Static.Encode as ",n]) netNames
            ,   "import Static.ServerTypes"
            ,   "import Data.Text as T"
            ,   "import Static.Types"
            ,   ""
            ,   "encodeOutgoingMessage :: NetOutgoingMessage -> T.Text"
            ,   "encodeOutgoingMessage netTrans ="
            ,   "    case netTrans of"
            ,   T.unlines $ map encodeCase netNames
            ]
        plugins :: T.Text
        plugins =
            let
                cmdCase netName = 
                    T.unlines
                    [
                        T.concat ["                ",netName,"Trans {} ->"]
                    ,   T.concat ["                      Cmd.process cmd centralMessageQueue (safeFromJust \"plugins\" $ TM.lookup $ serverState state :: NetState ",netName,".Player)"]
                    ] 
            in
            T.unlines
            [
                "module Static.Plugins where"
            ,   T.unlines $ map (\netName -> T.concat["import ",netName,".Static.Types as ",netName]) netNames
            ,   "import Static.ServerTypes"
            ,   "import Static.Types"
            ,   "import Data.Maybe (fromJust)"
            ,   "import Control.Concurrent.STM (TQueue, atomically, writeTQueue)"
            ,   "import Data.TMap as TM (TMap,lookup)"
            ,   "import Utils.Utils as Utils"
            ,   "import qualified Static.Cmd as Cmd"
            ,   ""
            ,   "processCmd :: TQueue CentralMessage -> Maybe (Cmd.Cmd NetTransition) -> NetTransition -> ServerState -> IO ()"
            ,   "processCmd centralMessageQueue mCmd nTrans state ="
            ,   "    case mCmd of"
            ,   "        Just cmd ->"
            ,   "            case nTrans of"
            ,   T.unlines $ map cmdCase netNames
            ,   "        Nothing -> return ()"
            ]

    in do
        createDirectoryIfMissing True (fp </> "server" </> "src" </> "Static")
        copyDirectory (rootDir </> "ServerTemplate/") (fp </> "server/")
        writeIfNew 0 (fp </> "server" </> "src" </> "Static" </> "Init" <.> "hs") init 
        writeIfNew 0 (fp </> "server" </> "src" </> "Static" </> "Types" <.> "hs") types 
        writeIfNew 0 (fp </> "server" </> "src" </> "Static" </> "Decode" <.> "hs") decode 
        writeIfNew 0 (fp </> "server" </> "src" </> "Static" </> "Encode" <.> "hs") encode 
        writeIfNew 0 (fp </> "server" </> "src" </> "Static" </> "Update" <.> "hs") update
        writeIfNew 0 (fp </> "server" </> "src" </> "Static" </> "Plugins" <.> "hs") plugins
        mapM_ (Generate.Net.Server.generate extraT fp) netLst