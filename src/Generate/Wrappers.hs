{-# LANGUAGE OverloadedStrings #-}

module Generate.Wrappers where

import Types
import qualified Data.Text as T
import Generate.Types

createUnwrap :: Bool -> String -> String -> Constructor -> T.Text
createUnwrap h outputType inputPrefix (n,args) =
    let
        (.::.) = if h then " :: " else " : "
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

createWrap :: Bool -> Bool -> String -> String -> Constructor -> T.Text
createWrap def h inputType outputPrefix (n,args) =
    let
        (.::.) = if h then " :: " else " : "
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
            ,   if def then T.concat["        _ -> error \"Tried to wrap a value at the wrong time!\""] else ""
            ,   ""
            ]