{-# LANGUAGE OverloadedStrings #-}

module Generate.Types where

import Types                 (Language(..),ElmDocType, ElmType(..), ElmCustom(..), Constructor)
import qualified Data.Map       as M
import qualified Data.Text      as T
import qualified Data.Set       as S
import Data.Maybe (mapMaybe)

data Deriving = 
    DOrd | DShow | DEq | DData | DTypeable | DCustom T.Text


deriv2Txt DOrd = "Ord"
deriv2Txt DShow = "Show"
deriv2Txt DEq = "Eq"
deriv2Txt DData = "Data"
deriv2Txt DTypeable = "Typeable"
deriv2Txt (DCustom d) = d
derivTxt deriv = T.concat ["(",T.intercalate "," $ map deriv2Txt deriv,")"]

generateType :: Language -> Bool -> [Deriving] -> ElmCustom -> T.Text
generateType l commentsEnabled deriv (ElmCustom typeName constrs) =
    let
        typ   = if l == Haskell then "data" else "type"

        constrs2Txt = map (generateConstructor l commentsEnabled) constrs
        typeParams = concatMap (\(_,constr) -> 
                        mapMaybe (\ (et,_,_) ->
                            case et of 
                                ElmWildcardType n -> Just $ T.pack n
                                _ -> Nothing                
                            ) constr
                        ) constrs
    in
        if length constrs > 0 then
        T.concat [ typ, " ", T.pack typeName, " ", T.intercalate " " typeParams ," =\n      "
                 , T.intercalate "\n    | " constrs2Txt
                 , if length deriv > 0 && l == Haskell then T.concat ["\n    deriving",derivTxt deriv] else ""
                 ]
        else ""

generateTypeAlias :: Language -> Bool -> [Deriving] -> String -> ElmType -> T.Text
generateTypeAlias l commentsEnabled deriv typeName et =
    let
        typ = if l == Haskell then "type" else "type alias"
    in
        T.concat [ typ, " ", T.pack typeName, " = ",T.pack typeName, " ", et2Txt l commentsEnabled et
                 , if length deriv > 0 && l == Haskell then T.concat ["   deriving",derivTxt deriv] else ""
                 ]

generateNewtype :: Bool -> [Deriving] -> String -> ElmType -> T.Text
generateNewtype commentsEnabled deriv typeName et =
    let
        typ = "newtype"
    in
        T.concat [ typ, " ", T.pack typeName, " = ", T.pack typeName," ",et2Txt Haskell commentsEnabled et
                 , if length deriv > 0 then T.concat ["   deriving",derivTxt deriv] else ""
                 ]

generateConstructor :: Language -> Bool -> Constructor -> T.Text
generateConstructor l commentsEnabled (constrName,elmDocTypes) =
    T.intercalate " " $ T.pack constrName : map (edt2Txt l commentsEnabled) elmDocTypes

edt2Txt :: Language -> Bool -> (ElmType, String, String) -> T.Text
edt2Txt l commentsEnabled (et, n, d) = T.concat [et2Txt l commentsEnabled et, if commentsEnabled then T.concat [" {-", T.pack n, "-}"] else ""]

edt2Pat :: (ElmType, String, String) -> T.Text
edt2Pat (et, n, d) = T.pack n

et2Txt :: Language -> Bool -> ElmType -> T.Text
et2Txt l c (ElmIntRange _ _)            = "Int"
et2Txt l c (ElmFloatRange _ _ _)        = if l == Haskell then "Double" else "Float"
et2Txt l c ElmString                    = "String"
et2Txt l c (ElmSizedString _)           = "String"
et2Txt l c (ElmPair edt0 edt1)          = T.concat ["(",edt2Txt l c edt0,", ",edt2Txt l c edt1,")"]
et2Txt l c (ElmTriple edt0 edt1 edt2)   = T.concat ["(",edt2Txt l c edt0,", ",edt2Txt l c edt1,", ",edt2Txt l c edt2,")"]
et2Txt l c (ElmList edt)                = T.concat ["(List ",edt2Txt l c edt,")"]
et2Txt l c (ElmDict edt0 edt1)          = T.concat ["(Dict ",edt2Txt l c edt0," ",edt2Txt l c edt1,")"]
et2Txt l c (ElmType name)               = T.pack name
et2Txt l c (ElmExisting name mod)       = T.concat[T.pack mod,".",T.pack name]
et2Txt l c (ElmExistingWParams name params mod) = T.concat["(",T.pack mod,".",T.pack name," ",T.intercalate " " $ map (\(typ,imp) -> T.pack $ imp ++ "." ++ typ) params,")"]
et2Txt l c (ElmWildcardType s)          = T.pack s
et2Txt l c (ElmMaybe edt)               = T.concat ["(Maybe ",edt2Txt l c edt,")"]
et2Txt l c ElmBool                      = T.concat ["Bool"]
et2Txt l c (ElmResult edt0 edt1)        = T.concat ["(Result ",edt2Txt l c edt0," ",edt2Txt l c edt1,")"]
et2Txt l c ElmEmpty                     = "()"

     
ec2Def :: M.Map String ElmCustom -> ElmCustom -> T.Text
ec2Def ecMap (ElmCustom name (fstConstr:_)) =
    T.concat ["(",constr2Def ecMap fstConstr,")"]
ec2Def ecMap (ElmCustom _ _) = 
    error "Custom data type has no constructors!"

constr2Def :: M.Map String ElmCustom -> Constructor -> T.Text
constr2Def ecMap (name,edts) =
    T.concat[T.pack name," ",T.intercalate " " $ map (\edt -> etd2Def ecMap edt) edts]

etd2Def :: M.Map String ElmCustom -> ElmDocType -> T.Text
etd2Def ecMap (ElmPair edt0 edt1, _, _)       = T.concat ["(",etd2Def ecMap edt0,", ",etd2Def ecMap edt1,")"]
etd2Def ecMap (ElmTriple edt0 edt1 edt2,_,_)  = T.concat ["(",etd2Def ecMap edt0,", ",etd2Def ecMap edt1,", ",etd2Def ecMap edt2,")"]
etd2Def ecMap (ElmResult edt _,_,_)           = T.concat ["(Err ",etd2Def ecMap edt,")"]
etd2Def ecMap (ElmType name,_,_)              = 
    case M.lookup name ecMap of
        Just ec -> ec2Def ecMap ec
        Nothing -> error $ "Custom data type " ++ name ++ " does not exist in map!"
etd2Def _ (ElmExisting name _, _, _)          = T.concat ["(error \"Please fill out default type for type",T.pack name,"\")"]
etd2Def _ (ElmWildcardType s,_,_)         = error "Unknown default type for type variable" --T.concat["error \"Error: default type unknown for type variable\""]
etd2Def _ (et,_,_)                        = et2Def et

et2Def :: ElmType -> T.Text
et2Def (ElmIntRange lo _)               = if lo < 0 then T.concat["(",T.pack $ show lo,")"] else T.pack $ show lo
et2Def (ElmFloatRange lo _ _)           = if lo < 0 then T.concat["(",T.pack $ show lo,")"] else T.pack $ show lo
et2Def ElmString                        = "\"\""
et2Def (ElmSizedString _)               = "\"\""
et2Def (ElmPair (etn0,_,_) (etn1,_,_))  = T.concat ["(",et2Def etn0,", ",et2Def etn1,")"]
et2Def (ElmTriple (etn0,_,_) (etn1,_,_) (etn2,_,_)) = T.concat ["(",et2Def etn0,", ",et2Def etn1,", ",et2Def etn2,")"]
et2Def (ElmList _)                      = "[]"
et2Def (ElmDict _ _)                    = "Dict.empty"
et2Def (ElmType name)                   = T.concat["default", T.pack name]
et2Def (ElmExisting name _)             = T.concat ["(error \"Please fill out default type for type",T.pack name,"\")"]
et2Def (ElmExistingWParams name _ _)    = T.concat ["(error \"Please fill out default type for type",T.pack name,"\")"]
et2Def (ElmWildcardType s)              = T.concat["default", T.pack s]
et2Def (ElmMaybe _)                     = "Nothing"
et2Def ElmBool                          = "False"
et2Def (ElmResult (et,_,_) _)           = T.concat["Err ",et2Def et]
et2Def ElmEmpty                         = "()"

generatePattern :: Constructor -> T.Text
generatePattern (constrName, args) =
    let
        patternTxt = T.intercalate " " $ map edt2Pat args
    in
        T.concat [if length args > 0 then "(" else "",T.pack constrName, " ", patternTxt, if length args > 0 then ") " else ""]