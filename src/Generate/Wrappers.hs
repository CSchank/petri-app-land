{-# LANGUAGE OverloadedStrings #-}

module Generate.Wrappers where

import Types
import qualified Data.Text as T
import Generate.Types
import qualified Data.Map as M
import TypeHelpers

createUnwrap :: Language -> T.Text -> T.Text -> Constructor -> T.Text
createUnwrap l outputType inputPrefix (n,args) =
    let
        (.::.) = if l == Haskell then " :: " else " : "
        name = T.concat ["unwrap", n]
        typE = T.concat [name, (.::.), n, " -> ", outputType]
        decl = T.concat [name," ",generatePattern (n,args)," = ",generatePattern (T.concat[inputPrefix,n],args)]
    in
        T.unlines
            [
                typE
            ,   decl
            ,   ""
            ]

createWrap :: M.Map T.Text CustomT -> Bool -> Language -> T.Text -> T.Text -> Constructor -> T.Text
createWrap ecMap def l inputType outputPrefix (n,args) =
    let
        (.::.) = if l == Haskell then " :: " else " : "
        name = T.concat ["wrap",n]
        typE = T.concat [name, (.::.),inputType," -> ",n]
        decl = T.concat [name," x__ ="]
    in
        T.unlines
            [
                typE
            ,   decl
            ,   "    case x__ of"
            ,   T.concat["        ",generatePattern (T.concat[outputPrefix,n],args)," -> ",generatePattern (n,args)]
            ,   if def then T.concat["        _ -> ",constr2Def ecMap (n,args)] else ""
            ,   ""
            ]

grouped = M.toList . M.fromListWith (\ a b -> a ++ b) . map (\(a,b) -> (a,[b]))


createTransitionUnwrap :: Bool -> Language -> Transition -> T.Text
createTransitionUnwrap def l (Transition transType (transName,_) connections mCmd) =
    let
        (.::.) = if l == Haskell then " :: " else " : "
        -- FIXME: don't repeat this code
        --genConstructors ::  [T.Text]
        genFunctions (from,toLst) =
            let
                name = T.concat ["unwrap", transName, "from",from]
                transitionName = T.concat [transName,"from",from]
                typE = T.concat [name, (.::.),transitionName," -> (Player, Maybe ClientMessage)"]
                decl = T.concat [name," trans ="]   
                genConstructors = map (\mTo -> 
                    case mTo of 
                        Just (to,(msgName,_),mCmd) ->
                            let 
                                (n,args) = constructor (T.concat[transName,"_",from,"to",to]) [dt (TypeT "") "player" "", dt (TypeT msgName) "msg" ""]
                            in
                                T.concat
                                    [
                                        T.concat ["        ",generatePattern (n,args), " -> (unwrap",to,"Player player, Just $ unwrap",msgName," msg)"]
                                    ]
                        Nothing -> 
                            let 
                                (n,args) = constructor (T.concat[transName,"_Stay_",from]) [dt (TypeT "") "player" ""]
                            in
                                T.concat
                                    [
                                        T.concat ["        ",generatePattern (n,args), " -> (unwrap",from,"Player player, Nothing)"]
                                    ]
                            ) toLst     
            in
            T.unlines
            [
                typE
                ,   decl
                ,   "    case trans of"
                --,   T.unlines $ map (\(n,args) -> T.concat["        ",generatePattern (n,args)," -> ",generatePattern (n,args)]) $ constrs
                ,   T.unlines $ genConstructors
                --,   if def then T.concat["        _ -> error \"Tried to wrap a value at the wrong time!\""] else ""
                ,   ""    
            ]
    in
        T.unlines $ map genFunctions (grouped connections)
createTransitionUnwrap _ _ _ = ""