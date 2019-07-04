{-# LANGUAGE OverloadedStrings #-}

module Generate.Types where

import Types                 (Language(..),DocTypeT, TypeT(..), CustomT(..), Constructor)
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

generateType :: Language -> Bool -> Bool -> [Deriving] -> CustomT -> T.Text
generateType l commentsEnabled generateDefault deriv (CustomT typeName constrs) =
    let
        typ   = if l == Haskell then "data" else "type"

        constrs2Txt = map (generateConstructor l commentsEnabled) constrs
        typeParams = concatMap (\(_,constr) -> 
                        mapMaybe (\ (et,_,_) ->
                            case et of 
                                WildcardTypeT n -> Just n
                                _ -> Nothing                
                            ) constr
                        ) constrs
    in
        if length constrs > 0 then
        T.concat [ typ, " ", typeName, " ", T.intercalate " " typeParams ," =\n      "
                 , T.intercalate "\n    | " constrs2Txt
                 , if length deriv > 0 && l == Haskell then T.concat ["\n    deriving",derivTxt deriv] else ""
                 ]
        else if generateDefault then
        T.concat [typ, " ", typeName, " = ", typeName]
        else ""

generateTypeAlias :: Language -> Bool -> [Deriving] -> String -> TypeT -> T.Text
generateTypeAlias l commentsEnabled deriv typeName et =
    let
        typ = if l == Haskell then "type" else "type alias"
    in
        T.concat [ typ, " ", T.pack typeName, " = ",T.pack typeName, " ", et2Txt l commentsEnabled et
                 , if length deriv > 0 && l == Haskell then T.concat ["   deriving",derivTxt deriv] else ""
                 ]

generateNewtype :: Bool -> [Deriving] -> T.Text -> TypeT -> T.Text
generateNewtype commentsEnabled deriv typeName et =
    let
        typ = "newtype"
    in
        T.concat [ typ, " ", typeName, " = ", typeName," ",et2Txt Haskell commentsEnabled et
                 , if length deriv > 0 then T.concat ["   deriving",derivTxt deriv] else ""
                 ]

generateConstructor :: Language -> Bool -> Constructor -> T.Text
generateConstructor l commentsEnabled (constrName,elmDocTypes) =
    T.intercalate " " $ constrName : map (edt2Txt l commentsEnabled) elmDocTypes

edt2Txt :: Language -> Bool -> DocTypeT -> T.Text
edt2Txt l commentsEnabled (et, n, d) = T.concat [et2Txt l commentsEnabled et, if commentsEnabled then T.concat [" {-", n, "-}"] else ""]

edt2Pat :: DocTypeT -> T.Text
edt2Pat (_, n, _) = n

et2Txt :: Language -> Bool -> TypeT -> T.Text
et2Txt l c (IntRangeT _ _)            = "Int"
et2Txt l c (FloatRangeT _ _ _)        = if l == Haskell then "Double" else "Float"
et2Txt l c StringT                    = "String"
et2Txt l c (SizedStringT _)           = "String"
et2Txt l c (PairT edt0 edt1)          = T.concat ["(",edt2Txt l c edt0,", ",edt2Txt l c edt1,")"]
et2Txt l c (TripleT edt0 edt1 edt2)   = T.concat ["(",edt2Txt l c edt0,", ",edt2Txt l c edt1,", ",edt2Txt l c edt2,")"]
et2Txt l c (ListT dt)                = T.concat ["(List ",edt2Txt l c dt,")"]
et2Txt l c (DictT edt0 edt1)          = T.concat ["(Dict ",edt2Txt l c edt0," ",edt2Txt l c edt1,")"]
et2Txt l c (TypeT name)               = name
et2Txt l c (ExistingT name mod)       = T.concat[mod,".", name]
et2Txt l c (ExistingWParamsT name params mod) = T.concat["(",mod,".", name," ",T.intercalate " " $ map (\(typ,imp) -> T.concat[imp,".",typ]) params,")"]
et2Txt l c (WildcardTypeT s)          = s
et2Txt l c (MaybeT dt)               = T.concat ["(Maybe ",edt2Txt l c dt,")"]
et2Txt l c BoolT                      = T.concat ["Bool"]
et2Txt l c (ResultT edt0 edt1)        = T.concat ["(Result ",edt2Txt l c edt0," ",edt2Txt l c edt1,")"]
et2Txt l c EmptyT                     = "()"

     
ec2Def :: M.Map T.Text CustomT -> CustomT -> T.Text
ec2Def ecMap (CustomT name (fstConstr:_)) =
    T.concat ["(",constr2Def ecMap fstConstr,")"]
ec2Def ecMap (CustomT _ _) = 
    error "Custom data type has no constructors!"

constr2Def :: M.Map T.Text CustomT -> Constructor -> T.Text
constr2Def ecMap (name,edts) =
    T.concat[name," ",T.intercalate " " $ map (\dt -> etd2Def ecMap dt) edts]

etd2Def :: M.Map T.Text CustomT -> DocTypeT -> T.Text
etd2Def ecMap (PairT edt0 edt1, _, _)       = T.concat ["(",etd2Def ecMap edt0,", ",etd2Def ecMap edt1,")"]
etd2Def ecMap (TripleT edt0 edt1 edt2,_,_)  = T.concat ["(",etd2Def ecMap edt0,", ",etd2Def ecMap edt1,", ",etd2Def ecMap edt2,")"]
etd2Def ecMap (ResultT dt _,_,_)           = T.concat ["(Err ",etd2Def ecMap dt,")"]
etd2Def ecMap (TypeT name,_,_)              = 
    case M.lookup name ecMap of
        Just ct -> ec2Def ecMap ct
        Nothing -> error $ "Custom data type " ++ T.unpack name ++ " does not exist in map!"
etd2Def _ (ExistingT name _, _, _)          = T.concat ["(error \"Please fill out default type for type",name,"\")"]
etd2Def _ (WildcardTypeT s,_,_)         = error "Unknown default type for type variable" --T.concat["error \"Error: default type unknown for type variable\""]
etd2Def _ (et,_,_)                        = et2Def et

et2Def :: TypeT -> T.Text
et2Def (IntRangeT lo _)               = if lo < 0 then T.concat["(",T.pack $ show lo,")"] else T.pack $ show lo
et2Def (FloatRangeT lo _ _)           = if lo < 0 then T.concat["(",T.pack $ show lo,")"] else T.pack $ show lo
et2Def StringT                        = "\"\""
et2Def (SizedStringT _)               = "\"\""
et2Def (PairT (etn0,_,_) (etn1,_,_))  = T.concat ["(",et2Def etn0,", ",et2Def etn1,")"]
et2Def (TripleT (etn0,_,_) (etn1,_,_) (etn2,_,_)) = T.concat ["(",et2Def etn0,", ",et2Def etn1,", ",et2Def etn2,")"]
et2Def (ListT _)                      = "[]"
et2Def (DictT _ _)                    = "Dict.empty"
et2Def (TypeT name)                   = T.concat["default", name]
et2Def (ExistingT name _)             = T.concat ["(error \"Please fill out default type for type",name,"\")"]
et2Def (ExistingWParamsT name _ _)    = T.concat ["(error \"Please fill out default type for type",name,"\")"]
et2Def (WildcardTypeT s)              = T.concat["default",s]
et2Def (MaybeT _)                     = "Nothing"
et2Def BoolT                          = "False"
et2Def (ResultT (et,_,_) _)           = T.concat["Err ",et2Def et]
et2Def EmptyT                         = "()"

generatePattern :: Constructor -> T.Text
generatePattern (constrName, args) =
    let
        patternTxt = T.intercalate " " $ map edt2Pat args
    in
        T.concat [if length args > 0 then "(" else "",constrName, " ", patternTxt, if length args > 0 then ") " else ""]