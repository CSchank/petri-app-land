{-# LANGUAGE OverloadedStrings #-}

module Generate.Types where

import Types                 (ElmDocType, ElmType(..), ElmCustom(..), ClientStateDiagram, ServerStateDiagram, ClientServerApp)
import qualified Data.Map       as M
import qualified Data.Text      as T
import qualified Data.Set       as S

generateClientStateType :: ClientStateDiagram -> T.Text
generateClientStateType client =
    let 
        csLst = M.toList client
        --stateType = 
    in 
        ""


elmCustomToClientType :: ElmCustom -> T.Text
elmCustomToClientType (ElmCustom typeName constrs) =
    let
        constrs2Txt = map (\(constrName, elmDocTypes) -> T.intercalate " " $ T.pack constrName : map edt2Txt elmDocTypes) constrs

        edt2Txt (ElmInt, _, _)                      = "Int"
        edt2Txt (ElmIntRange _ _, _, _)             = "Int"
        edt2Txt (ElmFloat, _, _)                    = "Float"
        edt2Txt (ElmFloatRange _ _ _, _, _)         = "Float"
        edt2Txt (ElmString, _, _)                   = "String"
        edt2Txt (ElmSizedString _, _, _)            = "String"
        edt2Txt (ElmPair edt0 edt1, _, _)           = T.concat ["(",edt2Txt edt0,", ",edt2Txt edt1,")"]
        edt2Txt (ElmTriple edt0 edt1 edt2, _, _)    = T.concat ["(",edt2Txt edt0,", ",edt2Txt edt1,", ",edt2Txt edt2,")"]
        edt2Txt (ElmList edt, _, _)                 = T.concat ["(List ",edt2Txt edt,")"]
        edt2Txt (ElmType (ElmCustom name _), _, _)  = T.pack name
    in
        T.concat [ "type ", T.pack typeName, " =\n\t  "
                 , T.intercalate "\n\t| " constrs2Txt
                 ]