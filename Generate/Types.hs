{-# LANGUAGE OverloadedStrings #-}

module Generate.Types where

import Types                 (ElmDocType, ElmType(..), ElmCustom(..), ClientStateDiagram, ServerStateDiagram, ClientServerApp, Constructor)
import qualified Data.Map       as M
import qualified Data.Text      as T
import qualified Data.Set       as S


generateType :: Bool -> Bool -> ElmCustom -> T.Text
generateType haskell commentsEnabled (ElmCustom typeName constrs) =
    let
        typ   = if haskell then "data" else "type"

        constrs2Txt = map (generateConstructor haskell commentsEnabled) constrs
    in
        T.concat [ typ, " ", T.pack typeName, " =\n      "
                 , T.intercalate "\n    | " constrs2Txt
                 ]

generateConstructor :: Bool -> Bool -> Constructor -> T.Text
generateConstructor haskell commentsEnabled (constrName,elmDocTypes) =
    T.intercalate " " $ T.pack constrName : map (edt2Txt haskell commentsEnabled) elmDocTypes

edt2Txt :: Bool -> Bool -> (ElmType, String, String) -> T.Text
edt2Txt h commentsEnabled (et, n, d) = T.concat [et2Txt h commentsEnabled et, if commentsEnabled then T.concat [" {-", T.pack n, "-}"] else ""]

edt2Pat :: (ElmType, String, String) -> T.Text
edt2Pat (et, n, d) = T.pack n

et2Txt :: Bool -> Bool -> ElmType -> T.Text
et2Txt h c (ElmIntRange _ _)            = "Int"
et2Txt h c (ElmFloatRange _ _ _)        = if h then "Double" else "Float"
et2Txt h c ElmString                    = "String"
et2Txt h c (ElmSizedString _)           = "String"
et2Txt h c (ElmPair edt0 edt1)          = T.concat ["(",edt2Txt h c edt0,", ",edt2Txt h c edt1,")"]
et2Txt h c (ElmTriple edt0 edt1 edt2)   = T.concat ["(",edt2Txt h c edt0,", ",edt2Txt h c edt1,", ",edt2Txt h c edt2,")"]
et2Txt h c (ElmList edt)                = T.concat ["(List ",edt2Txt h c edt,")"]
et2Txt h c (ElmDict edt0 edt1)          = T.concat ["(Dict ",edt2Txt h c edt0," ",edt2Txt h c edt1,")"]
et2Txt h c (ElmType name)               = T.pack name
{-
et2Def :: ElmType -> T.Text
et2Def (ElmIntRange _ _)            = "Int"
et2Def  (ElmFloatRange _ _ _)        = if h then "Double" else "Float"
et2Def h c ElmString                    = "String"
et2Def h c (ElmSizedString _)           = "String"
et2Def h c (ElmPair edt0 edt1)          = T.concat ["(",edt2Txt h c edt0,", ",edt2Txt h c edt1,")"]
et2Def h c (ElmTriple edt0 edt1 edt2)   = T.concat ["(",edt2Txt h c edt0,", ",edt2Txt h c edt1,", ",edt2Txt h c edt2,")"]
et2Def h c (ElmList edt)                = T.concat ["(List ",edt2Txt h c edt,")"]
et2Def h c (ElmDict edt0 edt1)          = T.concat ["(Dict ",edt2Txt h c edt0," ",edt2Txt h c edt1,")"]
et2Def h c (ElmType name)               = T.pack name
-}

generatePattern :: Constructor -> T.Text
generatePattern (constrName, args) =
    let
        patternTxt = T.intercalate " " $ map edt2Pat args
    in
        T.concat [if length args > 0 then "(" else "",T.pack constrName, " ", patternTxt, if length args > 0 then ") " else ""]

{-
generatePattern :: Bool -> Bool -> ElmCustom -> T.Text
generatePattern haskell commentsEnabled (ElmCustom typeName constrs) =
    let
        float = if haskell then "Double" else "Float"
        typ   = if haskell then "data" else "type"

        constrs2Txt = map (\(constrName, elmDocTypes) -> T.intercalate " " $ T.pack constrName : map (edt2Txt haskell commentsEnabled) elmDocTypes) constrs

        edt2Txt (et, n, d) = T.pack n
    in
        T.concat [ typ, " ", T.pack typeName, " =\n    "
                 , T.intercalate "\n    | " constrs2Txt
                 ]-}