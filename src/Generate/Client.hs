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
import                  Generate.Version
import                  System.Directory
import                  System.FilePath.Posix   ((</>),(<.>))
import                  Data.Maybe              (mapMaybe,fromMaybe,fromJust)
import                  Data.Time               (getCurrentTime)
import Data.Foldable (find)
import TypeHelpers

generateClient :: Bool -> FilePath -> FilePath -> ClientServerApp -> IO ()
generateClient gsvg rootDir fp 
                  (startNet
                  ,netLst
                  ,extraTlst
                  ) =
    let
        extraT = M.fromList $ map (\(CustomT n constrs) -> (n,CustomT n constrs)) extraTlst
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
                "module Static.Init exposing (..)"
            ,   "import Static.Types exposing(..)"
            ,   T.concat ["import ",startNet,".Static.Init"]
            ,   ""
            ,   "init : (NetModel, Cmd NetTransition)"
            ,   T.concat["init = (",startNet," ",startNet,".Static.Init.init, Cmd.none)"]
            ]
        types :: T.Text
        types = 
            let
                netUnion    = ct "NetModel" $ map (\nname -> constructor nname [dt (TypeT (T.concat[nname,".Static.Types.NetState"])) "" ""]) netNames
                netMsgUnion = ct "NetIncomingMessage" $ map (\nname -> constructor (T.concat[nname, "InMsg"]) [dt (TypeT $ T.concat [nname,".Static.Types.IncomingMessage"]) "" ""]) netNames
                netOutgoingMsgUnion = ct "NetTransition" $ map (\nname -> constructor (T.concat[nname, "Trans"]) [dt (TypeT $ T.concat[nname, ".Static.Types.Transition"]) "" ""]) netNames
            in
            T.unlines 
            [
                "module Static.Types exposing(..)"
            ,   T.unlines $ map (\n -> T.concat ["import ",n,".Static.Types"]) netNames
            ,   ""
            ,   "type alias TopLevelData = ()"
            ,   "-- a type identifying all of the nets in the server"
            ,   generateType Elm False [] netUnion
            ,   "-- a union type of all the nets and their incoming transitions"
            ,   generateType Elm False [] netMsgUnion
            ,   "-- a union type of all the nets and their outgoing transitions"
            ,   generateType Elm False [] netOutgoingMsgUnion
            ]
        decode :: T.Text
        decode = 
            let

            in
            T.unlines
            [
                "module Static.Decode exposing(..)"
            ,   "import Static.Types exposing(..)"
            ,   "import Utils.Utils exposing(..)"
            ,   T.unlines $ map (\n -> T.concat ["import ",n,".Static.Decode"]) netNames
            ,   ""
            ,   "decodeIncomingMessage : String -> NetModel -> Result String NetIncomingMessage"
            ,   "decodeIncomingMessage txt clientNet ="
            ,   "    case clientNet of"
            ,   T.concat $ map (\netName -> T.concat["        ",netName," _ -> rMap ",netName,"InMsg <| Tuple.first <| ",netName,".Static.Decode.decodeIncomingMessage (Err \"\",String.split \"\\u{0000}\" txt)"]) netNames
            ]
        update :: T.Text
        update = 
            let
                updateCase netName = 
                    let
                        newName = T.concat["new",netName,"State"]
                    in
                    T.unlines 
                    [
                        T.concat["            (",netName,"InMsg msg, ",netName," m) ->"]
                    ,            "                let"
                    ,   T.concat["                    (",newName,", mcmd) = ",netName,".update tld msg m"]
                    ,   T.concat["                    newClientState = ",netName," ",newName]
                    ,   T.concat["                in (newClientState, Cmd.map ",netName,"Trans mcmd)"]
                    ]
                ttCase netName = 
                    T.unlines 
                    [
                        T.concat["        ",netName,"Trans tr -> ",netName,".transitionType tr"]
                    ]
                o2iCase netName = 
                    T.unlines 
                    [
                        T.concat["        ",netName,"Trans tr -> Maybe.map ",netName,"InMsg <| ",netName,".outgoingToIncoming tr"]
                    ]
            in
            T.unlines
            [
                "module Static.Update exposing(..)"
            ,   T.unlines $ map (\n -> T.concat ["import ",n,".Static.Update as ",n]) netNames
            ,   "import Static.Types exposing(..)"
            ,   "import Maybe"
            ,   ""
            ,   "update : TopLevelData -> NetIncomingMessage -> NetModel -> (NetModel, Cmd NetTransition)"
            ,   "update tld netInMsg state ="
            ,   "    case (netInMsg,state) of"
            ,   T.unlines $ map updateCase netNames
            ,   if length netLst > 1 then "            _ -> (state, Cmd.none)" else ""
            ,   "outgoingToIncoming : NetTransition -> Maybe NetIncomingMessage"
            ,   "outgoingToIncoming trans ="
            ,   "    case trans of"
            ,   T.unlines $ map o2iCase netNames
            ]
        encode :: T.Text
        encode = 
            let
                encodeCase netName = T.concat["        ",netName,"Trans msg -> ",netName,".encodeTransition msg"]
            in
            T.unlines
            [
                "module Static.Encode exposing(..)"
            ,   T.unlines $ map (\n -> T.concat ["import ",n,".Static.Encode as ",n]) netNames
            ,   "import Static.Types exposing(NetTransition(..))"
            ,   ""
            ,   "encodeTransition : NetTransition -> Maybe String"
            ,   "encodeTransition netTrans ="
            ,   "    case netTrans of"
            ,   T.unlines $ map encodeCase netNames
            ]
        subs :: T.Text
        subs =
            let
                subCase netName = T.concat["        ",netName," m -> Sub.map ",netName,"Trans <| ",netName,".subs m"]
            in
            T.unlines
            [
                "module Static.Subs exposing(..)"
            ,   "import Static.Types exposing(..)"
            ,   T.unlines $ map (\netName -> T.concat["import ",netName,".Static.Subs as ",netName]) netNames
            ,   "subscriptions : NetModel -> Sub NetTransition"
            ,   "subscriptions model ="
            ,   "    case model of"
            ,   T.unlines $ map subCase netNames
            ]
        view :: T.Text
        view =
            let
                viewCase netName = T.concat["        ",netName," m -> Html.map ",netName,"Trans <| ",netName,".view m"]
                titlCase netName = T.concat["        ",netName," m -> ",netName,".title m"]
            in
            T.unlines
            [
                "module Static.View exposing(..)"
            ,   T.unlines $ map (\n -> T.concat ["import ",n,".Static.View as ",n]) netNames
            ,   "import Static.Types exposing(..)"
            ,   "import Html exposing(Html)"
            ,   "view : NetModel -> Html NetTransition"
            ,   "view model ="
            ,   "    case model of"
            ,   T.unlines $ map viewCase netNames
            ,   "title : NetModel -> String"
            ,   "title model ="
            ,   "    case model of"
            ,   T.unlines $ map titlCase netNames
            ]

    in do
        createDirectoryIfMissing True (fp </> "client" </> "src" </> "Static")
        let templateDir = (rootDir </> "ClientTemplate/")
        copyDirectory templateDir (fp </> "client/")
        writeIfNew 0 (fp </> "client" </> "src" </> "Static" </> "Init" <.> "elm") init 
        writeIfNew 0 (fp </> "client" </> "src" </> "Static" </> "Types" <.> "elm") types 
        writeIfNew 0 (fp </> "client" </> "src" </> "Static" </> "Decode" <.> "elm") decode 
        writeIfNew 0 (fp </> "client" </> "src" </> "Static" </> "Encode" <.> "elm") encode 
        writeIfNew 0 (fp </> "client" </> "src" </> "Static" </> "Update" <.> "elm") update 
        writeIfNew 0 (fp </> "client" </> "src" </> "Static" </> "Subs" <.> "elm") subs 
        writeIfNew 0 (fp </> "client" </> "src" </> "Static" </> "View" <.> "elm") view 
        writeIfNew 0 (fp </> "client" </> "src" </> "Static" </> "Subscriptions" <.> "elm") subs
        mapM_ (Generate.Net.Client.generate extraT fp) netLst