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
import                  Generate.Net
import                  System.Directory
import                  System.FilePath.Posix   ((</>),(<.>))
import                  Data.Maybe              (mapMaybe,fromMaybe,fromJust)
import                  Data.Time               (getCurrentTime)
import Data.Foldable (find)
import TypeHelpers

generateServer :: Bool -> Bool -> FilePath -> ClientServerApp -> IO ()
generateServer gsvg onlyStatic fp 
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
                "module Static.Init where"
            ,   T.unlines $ map (\n -> T.concat ["import ",n,".Static.Init"]) netNames
            ,   ""
            ,   T.concat["init = ",startNet,".Static.Init.init"]
            ]
        types :: T.Text
        types = 
            let
                netUnion    = ec "NetModel" $ map (\nname -> constructor (T.unpack nname) []) netNames
                netMsgUnion = ec "NetTransitions" $ map (\nname -> constructor (T.unpack nname ++ "Trans") [edt (ElmType $ T.unpack nname ++ ".Static.Types.Transition") "" ""]) netNames
            in
            T.unlines 
            [
                "module Static.Types where"
            ,   T.unlines $ map (\n -> T.concat ["import ",n,".Static.Types"]) netNames
            ,   ""
            ,   "-- a type identifying all of the nets in the server"
            ,   generateType True False [DShow,DOrd,DEq] netUnion
            ,   "-- a union type of all the nets and their transitions"
            ,   generateType True False [DShow,DOrd,DEq] netMsgUnion
            ]
        decode :: T.Text
        decode = 
            let

            in
            T.unlines
            [
                "module Static.Decode where"
            ,   "import Static.Types"
            ,   "import qualified Data.Text as T"
            ,   "import Utils.Utils"
            ,   T.unlines $ map (\n -> T.concat ["import ",n,".Static.Encode"]) netNames
            ,   ""
            ,   "decodeIncomingMessage :: T.Text -> NetModel -> Result T.Text NetTransitions"
            ,   "decodeIncomingMessage txt clientNet ="
            ,   "    case clientNet of"
            ,   T.concat $ map (\netName -> T.concat["        ",netName," -> rMap ",netName,"Trans $ fst $ ",netName,".Static.Decode.decodeIncomingMessage (Err \"\",T.splitOn \"\\0\" txt)"]) netNames
            ]

    in do
        createDirectoryIfMissing True (fp </> "server" </> "src" </> "Static")
        copyDirectory "ServerTemplate/" (fp </> "server/")
        writeIfNew 0 (fp </> "server" </> "src" </> "Static" </> "Init" <.> "hs") init 
        writeIfNew 0 (fp </> "server" </> "src" </> "Static" </> "Types" <.> "hs") types 
        writeIfNew 0 (fp </> "server" </> "src" </> "Static" </> "Decode" <.> "hs") decode 
        mapM_ (generateServerNet sExtraT fp) netLst