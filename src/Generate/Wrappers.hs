{-# LANGUAGE OverloadedStrings #-}

module Generate.Wrappers where

import Types
import qualified Data.Text as T
import Generate.Types
import qualified Data.Map as M
import TypeHelpers

createUnwrap :: Language -> String -> String -> Constructor -> T.Text
createUnwrap l outputType inputPrefix (n,args) =
    let
        (.::.) = if l == Haskell then " :: " else " : "
        name = T.concat ["unwrap", T.pack n]
        typE = T.concat [name, (.::.), T.pack n, " -> ", T.pack outputType]
        decl = T.concat [name," ",generatePattern (n,args)," = ",generatePattern (inputPrefix++n,args)]
    in
        T.unlines
            [
                typE
            ,   decl
            ,   ""
            ]

createWrap :: M.Map String ElmCustom -> Bool -> Language -> String -> String -> Constructor -> T.Text
createWrap ecMap def l inputType outputPrefix (n,args) =
    let
        (.::.) = if l == Haskell then " :: " else " : "
        name = T.concat ["wrap", T.pack n]
        typE = T.concat [name, (.::.),T.pack inputType," -> ",T.pack n]
        decl = T.concat [name," x__ ="]
    in
        T.unlines
            [
                typE
            ,   decl
            ,   "    case x__ of"
            ,   T.concat["        ",generatePattern (outputPrefix++n,args)," -> ",generatePattern (n,args)]
            ,   if def then T.concat["        _ -> ",constr2Def ecMap (n,args)] else ""
            ,   ""
            ]

grouped = M.toList . M.fromListWith (\ a b -> a ++ b) . map (\(a,b) -> (a,[b]))


createTransitionUnwrap :: Bool -> Language -> NetTransition -> T.Text
createTransitionUnwrap def l (NetTransition transType (transName,_) connections mCmd) =
    let
        (.::.) = if l == Haskell then " :: " else " : "
        transTxt = T.pack transName
        -- FIXME: don't repeat this code
        --genConstructors ::  [T.Text]
        genFunctions (from,toLst) =
            let
                name = T.concat ["unwrap", T.pack transName, "from",from]
                transitionName = T.concat [T.pack transName,"from",from]
                typE = T.concat [name, (.::.),transitionName," -> (Player, Maybe ClientMessage)"]
                decl = T.concat [name," trans ="]   
                genConstructors = map (\mTo -> 
                    case mTo of 
                        Just (to,(msgName,_)) ->
                            let 
                                (n,args) = constructor (T.unpack $ T.concat[transTxt,"_",from,"to",to]) [edt (ElmType "") "player" "", edt (ElmType msgName) "msg" ""]
                            in
                                T.concat
                                    [
                                        T.concat ["        ",generatePattern (n,args), " -> (unwrap",to,"Player player, Just $ unwrap",T.pack msgName," msg)"]
                                    ]
                        Nothing -> 
                            let 
                                (n,args) = constructor (T.unpack $ T.concat[transTxt,"_Stay_",from]) [edt (ElmType "") "player" ""]
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