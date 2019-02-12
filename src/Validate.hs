{-# LANGUAGE OverloadedStrings #-}

module Validate where

import Types
import qualified Data.Text as T
import qualified Data.Set as S
import Utils
import Data.Maybe (fromMaybe)
import Data.List ((\\))


validateType :: String -> Bool
validateType name@(h:rest) =
    h `elem` ['A'..'Z']

validateSymbol :: String -> Bool
validateSymbol name@(h:rest) =
    h `elem` (['a'..'z'] ++ "_")

validateEdt :: S.Set String -> String -> ElmDocType -> [String]
validateEdt ecSet note (et,"",_) = [note++": empty data name for ElmDocType with ElmType " ++ show et]
validateEdt ecSet note (et,name@(h:rest),_) =
    if validateSymbol name
        then [] ++ validateEt ecSet (note++", in `"++name++"`") et
    else
        [(note++", in "++name)++": invalid name for type"]
        ++ validateEt ecSet (note++", in "++name) et

validateEt :: S.Set String -> String -> ElmType -> [String]
validateEt ecSet note (ElmIntRange lo hi) =
    if lo > hi then [note ++ ": lower bound on IntRange should not be greater than upper bound"] else []
validateEt ecSet note (ElmFloatRange lo hi prec) = 
    if lo > hi then  [note ++ ": lower bound on FloatRange should not be greater than upper bound"] else [] ++
    if prec < 1 then [note ++ ": float precision must be a number greater than zero"] else []
validateEt ecSet note ElmString = []
validateEt ecSet note (ElmSizedString n) = 
    if n < 0 then 
        [note ++ ": sized string cannot have a size less than zero"] 
    else []
validateEt ecSet note (ElmPair edt0 edt1) = 
    validateEdt ecSet (note++", in first element of pair") edt0 ++ 
    validateEdt ecSet (note++", in second element of pair") edt1
validateEt ecSet note (ElmTriple edt0 edt1 edt2) = 
    validateEdt ecSet (note++", in first element of triple") edt0 ++ 
    validateEdt ecSet (note++", in second element of triple") edt1 ++ 
    validateEdt ecSet (note++", in third element of triple") edt2
validateEt ecSet note (ElmList edt) = validateEdt ecSet (note++", in list element") edt
validateEt ecSet note (ElmDict edt0 edt1) = 
    (case edt0 of
        (ElmType _,_,_) -> [note++": Elm does not support custom types as dictionary keys"]
        _ -> [])
    ++ validateEdt ecSet (note ++ ", in dict key") edt0
    ++ validateEdt ecSet (note ++ ", in dict value") edt1
validateEt ecSet note (ElmType name@(h:rest)) =
    if not (h `elem` ['A'..'Z']) then
        [note++", in Type `"++name++"`: invalid name for type"]
    else [] ++
    if not (name `S.member` ecSet) then
        [note++", in Type `" ++name ++ "`: custom type does not exist"]
    else []
validateEt ecSet note (ElmWildcardType str) = [note++": in "++str++" type parameters not supported"] -- a type parameter
validateEt ecSet note (ElmMaybe edt) = validateEdt ecSet (note++", in Maybe constructor") edt
validateEt ecSet note ElmBool = []
validateEt ecSet note (ElmResult edt0 edt1) =
       validateEdt ecSet (note ++ ", in Error result") edt0
    ++ validateEdt ecSet (note ++ ", in Ok result") edt1

validateConstructor :: S.Set String -> String -> Constructor -> [String]
validateConstructor ecSet note ("",edts) = 
    concat (map (validateEdt ecSet (note++", in unnamed constructor")) edts)
validateConstructor ecSet note (name,edts) =
    if not $ validateType name then
        [(note++", in constructor `"++name)++"`"++": invalid constructor name"] ++ 
        concat (map (validateEdt ecSet (note++", in constructor `"++name++"`")) edts)
    else
        concat (map (validateEdt ecSet (note++", in constructor `"++name++"`")) edts)

validateElmCustom :: S.Set String -> String -> ElmCustom -> [String]
validateElmCustom ecSet note (ElmCustom "" constrs) = 
    concat (map (validateConstructor ecSet (note++", in unnamed custom type")) constrs)
validateElmCustom ecSet note (ElmCustom name constrs) =
    if not $ validateType name then
        [(note++", in custom type `"++name)++"`: invalid custom type name"] ++ 
        concatMap (validateConstructor ecSet (note++", in custom type `"++name++"`")) constrs
    else
        concatMap (validateConstructor ecSet (note++", in custom type `"++name++"`")) constrs

validatePlace :: S.Set String -> String -> HybridPlace -> [String]
validatePlace ecSet note (HybridPlace "" _ _ _ _ _ _) =
    [note++": a HybridPlace has an empty name"]
validatePlace ecSet note (HybridPlace name serverState playerState clientState mSubnet initCmds cSubs) =
    let
        validations =
            concatMap (validateEdt ecSet (note++", in place `"++T.unpack name++"`, in server state")) serverState ++
            concatMap (validateEdt ecSet (note++", in place `"++T.unpack name++"`, in player state")) playerState ++
            concatMap (validateEdt ecSet (note++", in place `"++T.unpack name++"`, in client state")) clientState 
    in
        if not $ validateType $ T.unpack name then
            [note++": invalid name for HybridPlace: `"++T.unpack name] ++ validations
        else
            validations

validateTransition :: S.Set String -> S.Set String -> String -> NetTransition -> [String]
validateTransition placeSet ecSet note (NetTransition _ constr@(transName,edts) fromtoLst mCmd) =
    let
        fromTos :: [(String,String)]
        fromTos = 
            map (\(from,mTo) -> (T.unpack from,T.unpack $ fromMaybe "" $ fmap fst mTo)) fromtoLst

        duplicateFromTo :: [(String,String)]
        duplicateFromTo =
            fromTos \\ (S.toList $ S.fromList fromTos)

        validateFromTo :: (T.Text, Maybe (T.Text, Constructor)) -> [String]
        validateFromTo (from,mTo) = 
            (if not (T.unpack from `S.member` placeSet) then
                [note ++", in from transition: place `"++T.unpack from++"` does not exist"]
            else []) ++
                (case mTo of
                    Just (to,constr) ->
                        if not (T.unpack to `S.member` placeSet) then
                            [note ++", in to transition: place `"++T.unpack to++"` does not exist"]
                        else []
                    Nothing -> [])
    in
        validateConstructor ecSet (note++", in transition `"++transName++"`") constr ++
        concatMap validateFromTo fromtoLst ++
        map (\tr -> note ++ ", in transition `"++transName++"`: duplicate transition "++show tr) duplicateFromTo
validateTransition placeSet ecSet note (ClientTransition constr@(transName,edts) placeName mCmd) =
        validateConstructor ecSet (note++", in transition `"++transName++"`") constr ++
        if not $ validateType (T.unpack placeName) then
            ["invalid name for Client Transition `"++transName++"`"]
        else []



validateNet :: S.Set String -> Net -> [String]
validateNet ecSet (HybridNet name startingPlace places transitions subnets) =
    let
        placeSet = S.fromList $ map (T.unpack . getPlaceName) places
        validations =
            if not (T.unpack startingPlace `S.member` placeSet) then
                ["in Net `" ++ T.unpack name ++ "`: start place `"++ T.unpack startingPlace ++"` not in place list"]
            else [] ++
            concatMap (validatePlace ecSet ("in Net `"++T.unpack name ++"`")) places ++
            concatMap (validateTransition placeSet ecSet ("in Net `"++T.unpack name ++"`")) transitions
    in
        if not $ validateType $ T.unpack name then
            ["in Net `"++T.unpack name++"`: invalid name for Net"] ++ validations
        else
            validations

validateCSApp :: ClientServerApp -> [String]
validateCSApp 
    ( startNet
    , nets
    , extraTLst
    )
    =
    let
        ecSet = S.fromList $ map (\(ElmCustom n _) -> n) extraTLst
        netSet = S.fromList $ map (\(HybridNet n _ _ _ _) -> n) nets
        validations =
            concatMap (validateNet ecSet) nets ++
            concatMap (validateElmCustom ecSet ("in extra types")) extraTLst
    in
        if not (validateType $ T.unpack startNet) then
            ["invalid name for starting net `"++T.unpack startNet++"`"] ++ validations
        else 
            validations