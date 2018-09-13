{-# LANGUAGE OverloadedStrings #-}

module Generate.Types where

import Types                 (ElmDocType, ElmType(..), ElmCustom(..), ClientStateDiagram, ServerStateDiagram, ClientServerApp)
import qualified Data.Map       as M
import qualified Data.Text      as T
import qualified Data.Set       as S

generateClientStateTypes :: M.Map String ElmCustom -> Bool -> T.Text
generateClientStateTypes clientTypes commentsEnabled =
    T.intercalate "\n\n" $ map (\(name,elmCustom) -> elmCustomToClientType commentsEnabled elmCustom) (M.toList clientTypes)


elmCustomToClientType :: Bool -> ElmCustom -> T.Text
elmCustomToClientType commentsEnabled (ElmCustom typeName constrs) =
    let
        constrs2Txt = map (\(constrName, elmDocTypes) -> T.intercalate " " $ T.pack constrName : map edt2Txt elmDocTypes) constrs

        edt2Txt (et, n, d) = T.concat [et2Txt et, if commentsEnabled then T.concat [" {-", T.pack n, "-}"] else ""]

        et2Txt (ElmIntRange _ _)            = "Int"
        et2Txt (ElmFloatRange _ _ _)        = "Float"
        et2Txt ElmString                    = "String"
        et2Txt (ElmSizedString _)           = "String"
        et2Txt (ElmPair edt0 edt1)          = T.concat ["(",edt2Txt edt0,", ",edt2Txt edt1,")"]
        et2Txt (ElmTriple edt0 edt1 edt2)   = T.concat ["(",edt2Txt edt0,", ",edt2Txt edt1,", ",edt2Txt edt2,")"]
        et2Txt (ElmList edt)                = T.concat ["(List ",edt2Txt edt,")"]
        et2Txt (ElmType name)               = T.pack name
    in
        T.concat [ "type ", T.pack typeName, " =\n\t  "
                 , T.intercalate "\n\t| " constrs2Txt
                 ]

elmCustomToServerType :: Bool -> ElmCustom -> T.Text
elmCustomToServerType commentsEnabled (ElmCustom typeName constrs) =
    let
        constrs2Txt = map (\(constrName, elmDocTypes) -> T.intercalate " " $ T.pack constrName : map edt2Txt elmDocTypes) constrs

        edt2Txt (et, n, d) = T.concat [et2Txt et, if commentsEnabled then T.concat [" {-", T.pack n, "-}"] else ""]

        et2Txt (ElmIntRange _ _)            = "Int"
        et2Txt (ElmFloatRange _ _ _)        = "Double"
        et2Txt ElmString                    = "String"
        et2Txt (ElmSizedString _)           = "String"
        et2Txt (ElmPair edt0 edt1)          = T.concat ["(",edt2Txt edt0,", ",edt2Txt edt1,")"]
        et2Txt (ElmTriple edt0 edt1 edt2)   = T.concat ["(",edt2Txt edt0,", ",edt2Txt edt1,", ",edt2Txt edt2,")"]
        et2Txt (ElmList edt)                = T.concat ["(List ",edt2Txt edt,")"]
        et2Txt (ElmType name)               = T.pack name
    in
        T.concat [ "data ", T.pack typeName, " =\n\t  "
                 , T.intercalate "\n\t| " constrs2Txt
                 ]