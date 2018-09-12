{-# LANGUAGE OverloadedStrings #-}

module Generate.Types where

import Types                 (ElmDocType, ElmType(..), ElmCustom(..), ClientStateDiagram, ServerStateDiagram, ClientServerApp)
import qualified Data.Map       as M
import qualified Data.Text      as T
import qualified Data.Set       as S

generateClientStateTypes :: ClientStateDiagram -> T.Text
generateClientStateTypes client =
    let 
        csLst = M.toList client
        --stateType = 
    in 
        ""


elmCustomToClientTypes :: ElmCustom -> [(String, [(String,[ElmDocType])])]
elmCustomToClientTypes (ElmCustom typeName typeConstrs) =
    let 
        findUniqueTypes (ElmType (ElmCustom name constrs), _, _) = (typeName, typeConstrs) : (name, constrs) : (concat $ map (\(name, inputs) -> concat $ map findUniqueTypes inputs) constrs) -- map uniqueTypes constrs
        findUniqueTypes _ = []

        uniqueTypes = M.toList $ M.fromList $ findUniqueTypes (ElmType $ ElmCustom typeName typeConstrs, "", "")
    in
        uniqueTypes

elmCustomToClientType :: ElmCustom -> T.Text
elmCustomToClientType (ElmCustom typeName constrs) =
    let
        constrs2Txt = map (\(constrName, elmDocTypes) -> T.intercalate " " $ T.pack constrName : map edt2Txt elmDocTypes) constrs

        edt2Txt (ElmInt, n, d)                      = "Int"
        edt2Txt (ElmIntRange _ _, n, d)             = "Int"
        edt2Txt (ElmFloat, n, d)                    = "Float"
        edt2Txt (ElmFloatRange _ _ _, n, d)         = "Float"
        edt2Txt (ElmString, n, d)                   = "String"
        edt2Txt (ElmSizedString _, n, d)            = "String"
        edt2Txt (ElmPair edt0 edt1, n, d)           = T.concat ["(",edt2Txt edt0,", ",edt2Txt edt1,")"]
        edt2Txt (ElmTriple edt0 edt1 edt2, n, d)    = T.concat ["(",edt2Txt edt0,", ",edt2Txt edt1,", ",edt2Txt edt2,")"]
        edt2Txt (ElmList edt, n, d)                 = T.concat ["(List ",edt2Txt edt,")"]
        edt2Txt (ElmType (ElmCustom name _), n, d)  = T.pack name
    in
        T.concat [ "type ", T.pack typeName, " =\n\t  "
                 , T.intercalate "\n\t| " constrs2Txt
                 ]