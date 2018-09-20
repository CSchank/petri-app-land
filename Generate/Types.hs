{-# LANGUAGE OverloadedStrings #-}

module Generate.Types where

import Types                 (ElmDocType, ElmType(..), ElmCustom(..), ClientStateDiagram, ServerStateDiagram, ClientServerApp)
import qualified Data.Map       as M
import qualified Data.Text      as T
import qualified Data.Set       as S


generateType :: Bool -> Bool -> ElmCustom -> T.Text
generateType haskell commentsEnabled (ElmCustom typeName constrs) =
    let
        float = if haskell then "Double" else "Float"
        typ   = if haskell then "data" else "type"

        constrs2Txt = map (\(constrName, elmDocTypes) -> T.intercalate " " $ T.pack constrName : map edt2Txt elmDocTypes) constrs

        edt2Txt (et, n, d) = T.concat [et2Txt et, if commentsEnabled then T.concat [" {-", T.pack n, "-}"] else ""]

        et2Txt (ElmIntRange _ _)            = "Int"
        et2Txt (ElmFloatRange _ _ _)        = float
        et2Txt ElmString                    = "String"
        et2Txt (ElmSizedString _)           = "String"
        et2Txt (ElmPair edt0 edt1)          = T.concat ["(",edt2Txt edt0,", ",edt2Txt edt1,")"]
        et2Txt (ElmTriple edt0 edt1 edt2)   = T.concat ["(",edt2Txt edt0,", ",edt2Txt edt1,", ",edt2Txt edt2,")"]
        et2Txt (ElmList edt)                = T.concat ["(List ",edt2Txt edt,")"]
        et2Txt (ElmDict edt0 edt1)          = T.concat ["(Dict ",edt2Txt edt0," ",edt2Txt edt1,")"]
        et2Txt (ElmType name)               = T.pack name
    in
        T.concat [ typ, " ", T.pack typeName, " =\n      "
                 , T.intercalate "\n    | " constrs2Txt
                 ]

generatePattern :: Bool -> Bool -> ElmCustom -> T.Text
generatePattern haskell commentsEnabled (ElmCustom typeName constrs) =
    let
        float = if haskell then "Double" else "Float"
        typ   = if haskell then "data" else "type"

        constrs2Txt = map (\(constrName, elmDocTypes) -> T.intercalate " " $ T.pack constrName : map edt2Txt elmDocTypes) constrs

        edt2Txt (et, n, d) = T.pack n
    in
        T.concat [ typ, " ", T.pack typeName, " =\n    "
                 , T.intercalate "\n    | " constrs2Txt
                 ]