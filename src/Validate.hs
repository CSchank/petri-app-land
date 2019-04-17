{-# LANGUAGE OverloadedStrings #-}

module Validate where

import Types
import qualified Data.Text as T
import qualified Data.Set as S
import Utils
import Data.Maybe (fromMaybe,mapMaybe)
import Data.List ((\\))


validateType :: T.Text -> Bool
validateType txt =
    let
        name@(h:rest) = T.unpack txt
    in
    h `elem` ['A'..'Z']

validateSymbol :: String -> Bool
validateSymbol name@(h:rest) =
    h `elem` (['a'..'z'] ++ "_")
validateSymbol "" = False

validateEdt :: S.Set T.Text -> String -> DocTypeT -> [String]
validateEdt ecSet note (et,"",_) = [note++": empty data name for DocTypeT with TypeT " ++ show et]
validateEdt ecSet note (et,nameTxt,_) =
    let
        name@(h:rest) = T.unpack nameTxt
    in
    if validateSymbol name
        then [] ++ validateEt ecSet (note++", in `"++name++"`") et
    else
        [(note++", in "++name)++": invalid name for type"]
        ++ validateEt ecSet (note++", in "++name) et

validateEt :: S.Set T.Text -> String -> TypeT -> [String]
validateEt ecSet note (IntRangeT lo hi) =
    if lo > hi then [note ++ ": lower bound on IntRange should not be greater than upper bound"] else []
validateEt ecSet note (FloatRangeT lo hi prec) = 
    if lo > hi then  [note ++ ": lower bound on FloatRange should not be greater than upper bound"] else [] ++
    if prec < 1 then [note ++ ": float precision must be a number greater than zero"] else []
validateEt ecSet note StringT = []
validateEt ecSet note (SizedStringT n) = 
    if n < 0 then 
        [note ++ ": sized string cannot have a size less than zero"] 
    else []
validateEt ecSet note (PairT edt0 edt1) = 
    validateEdt ecSet (note++", in first element of pair") edt0 ++ 
    validateEdt ecSet (note++", in second element of pair") edt1
validateEt ecSet note (TripleT edt0 edt1 edt2) = 
    validateEdt ecSet (note++", in first element of triple") edt0 ++ 
    validateEdt ecSet (note++", in second element of triple") edt1 ++ 
    validateEdt ecSet (note++", in third element of triple") edt2
validateEt ecSet note (ListT dt) = validateEdt ecSet (note++", in list element") dt
validateEt ecSet note (DictT edt0 edt1) = 
    (case edt0 of
        (TypeT _,_,_) -> [note++": Elm does not support custom types as dictionary keys"]
        _ -> [])
    ++ validateEdt ecSet (note ++ ", in dict key") edt0
    ++ validateEdt ecSet (note ++ ", in dict value") edt1
validateEt ecSet note (TypeT nameTxt) =
    let
        name@(h:rest) = T.unpack nameTxt
    in
    if not (h `elem` ['A'..'Z']) then
        [note++", in Type `"++name++"`: invalid name for type"]
    else [] ++
    if not (nameTxt `S.member` ecSet) then
        [note++", in Type `" ++name ++ "`: custom type does not exist"]
    else []
validateEt ecSet note (WildcardTypeT str) = [note++": in "++T.unpack str++" type parameters not supported"] -- a type parameter
validateEt ecSet note (MaybeT dt) = validateEdt ecSet (note++", in Maybe constructor") dt
validateEt ecSet note BoolT = []
validateEt ecSet note (ResultT edt0 edt1) =
       validateEdt ecSet (note ++ ", in Error result") edt0
    ++ validateEdt ecSet (note ++ ", in Ok result") edt1
validateEt ecSet note EmptyT = []
validateEt ecSet note (ExistingT str imp) = 
    (if not $ validateType str then
        [note++", in ExistingT type `"++T.unpack str++"`: invalid name for type"]
    else 
        []) ++
    if imp == "" then
        [note++", in ExistingT type `"++T.unpack str++"`: import must be non-empty and valid"]
    else
        []
validateEt ecSet note (ExistingWParamsT str params imp) = 
    (if not $ validateType str then
        [note++", in ExistingWParamT type `"++T.unpack str++"`: invalid name for type"]
    else 
        []) ++
    if imp == "" then
        [note++", in ExistingWParamT type `"++T.unpack str++"`: import must be non-empty and valid"]
    else
        [] ++
        mapMaybe (\(param, imp) -> 
            if not $ validateType param then 
                Just $ note++", in ExistingWParamT type `"++T.unpack str++"`, in type parameter `"++T.unpack param++"`: invalid name for parameter"
            else if not $ validateType imp then
                Just $ note++", in ExistingWParamT type `"++T.unpack str++"`, in type parameter `"++T.unpack param++"`, in import for type parameter `"++T.unpack imp++"`: invalid name for import"
            else
                Nothing) params

validateConstructor :: S.Set T.Text -> String -> Constructor -> [String]
validateConstructor ecSet note ("",edts) = 
    concat (map (validateEdt ecSet (note++", in unnamed constructor")) edts)
validateConstructor ecSet note (name,edts) =
    if not $ validateType name then
        [(note++", in constructor `"++T.unpack name)++"`"++": invalid constructor name"] ++ 
        concat (map (validateEdt ecSet (note++", in constructor `"++T.unpack name++"`")) edts)
    else
        concat (map (validateEdt ecSet (note++", in constructor `"++T.unpack name++"`")) edts)

validateElmCustom :: S.Set T.Text -> String -> CustomT -> [String]
validateElmCustom ecSet note (CustomT "" constrs) = 
    concat (map (validateConstructor ecSet (note++", in unnamed custom type")) constrs)
validateElmCustom ecSet note (CustomT name constrs) =
    if not $ validateType name then
        [(note++", in custom type `"++T.unpack name)++"`: invalid custom type name"] ++ 
        concatMap (validateConstructor ecSet (note++", in custom type `"++T.unpack name++"`")) constrs
    else
        concatMap (validateConstructor ecSet (note++", in custom type `"++T.unpack name++"`")) constrs

validatePlace :: S.Set T.Text -> String -> Place -> [String]
validatePlace ecSet note (Place "" _ _ _ _) =
    [note++": a Place has an empty name"]
validatePlace ecSet note (Place name serverState playerState clientState initCmd) =
    let
        cmdValidation = 
            case initCmd of
                Just cmd ->
                    if not $ validateType cmd then
                        [note++", in server command `"++T.unpack cmd++"`: invalid name for command"]
                    else []
                Nothing ->
                    []
        
        validations =
            concatMap (validateEdt ecSet (note++", in place `"++T.unpack name++"`, in server state")) serverState ++
            concatMap (validateEdt ecSet (note++", in place `"++T.unpack name++"`, in player state")) playerState ++
            concatMap (validateEdt ecSet (note++", in place `"++T.unpack name++"`, in client state")) clientState ++
            cmdValidation
    in
        if not $ validateType name then
            [note++": invalid name for Place: `"++T.unpack name] ++ validations
        else
            validations

validateTransition :: S.Set T.Text -> S.Set T.Text -> String -> Transition -> [String]
validateTransition placeSet ecSet note (Transition _ constr@(transName,edts) fromtoLst mCmd) =
    let
        fromTos :: [(String,String)]
        fromTos = 
            map (\(from,mTo) -> (T.unpack from,T.unpack $ fromMaybe "" $ fmap fst mTo)) fromtoLst

        duplicateFromTo :: [(String,String)]
        duplicateFromTo =
            fromTos \\ (S.toList $ S.fromList fromTos)

        validateFromTo :: (T.Text, Maybe (T.Text, Constructor)) -> [String]
        validateFromTo (from,mTo) = 
            (if not (from `S.member` placeSet) then
                [note ++", in from transition: place `"++T.unpack from++"` does not exist"]
            else []) ++
                (case mTo of
                    Just (to,constr) ->
                        (if not (to `S.member` placeSet) then
                            [note ++", in to transition: place `"++T.unpack to++"` does not exist"]
                        else [])
                        ++ validateConstructor ecSet (note++", in transition `"++T.unpack transName++"`") constr
                    Nothing -> [])

    in
        validateConstructor ecSet (note++", in transition `"++T.unpack transName++"`") constr ++
        concatMap validateFromTo fromtoLst ++
        map (\tr -> note ++ ", in transition `"++T.unpack transName++"`: duplicate transition "++show tr) duplicateFromTo

validateTransition placeSet ecSet note (ClientTransition constr@(transName,edts) placeName) =
        validateConstructor ecSet (note++", in transition `"++T.unpack transName++"`") constr ++
        if not $ validateType placeName then
            ["invalid name for Client Transition `"++T.unpack transName++"`"]
        else []

validateTransition placeSet ecSet note (CmdTransition constr@(transName,edts) placeName cmd) =
        validateConstructor ecSet (note++", in cmd transition `"++T.unpack transName++"`") constr ++
        if not $ validateType placeName then
            ["invalid name for Cmd Transition `"++T.unpack transName++"`"]
        else []
--TODO: check if commands exist



validateNet :: S.Set T.Text -> Net -> [String]
validateNet ecSet (Net name startingPlace places transitions subnets) =
    let
        placeSet = S.fromList $ map getPlaceName places
        duplicateTransitions = fnub $ transitions \\ fnub transitions
        duplicatePlaces = fnub $ places \\ fnub places
        validations =
            if not (startingPlace `S.member` placeSet) then
                ["in Net `" ++ T.unpack name ++ "`: start place `"++ T.unpack startingPlace ++"` not in place list"]
            else [] ++
            concatMap (validatePlace ecSet ("in Net `"++T.unpack name ++"`")) places ++
            concatMap (validateTransition placeSet ecSet ("in Net `"++T.unpack name ++"`")) transitions ++
            if length duplicateTransitions > 0 then
                map (\tr -> "in Net `"++T.unpack name ++"`, transition "++T.unpack (getTransitionName tr)++" appears " ++ show (length (filter (==tr) transitions)) ++ " times.") duplicateTransitions
            else
                []++
            if length duplicatePlaces > 0 then
                map (\pl -> "in Net `"++T.unpack name ++"`, place "++T.unpack (getPlaceName pl)++" appears " ++ show (length (filter (==pl) places)) ++ " times.") duplicatePlaces
            else
                []

    in
        if not $ validateType name then
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
        ecSet = S.fromList $ map (\(CustomT n _) -> n) extraTLst
        netSet = S.fromList $ map (\(Net n _ _ _ _) -> n) nets
        validations =
            concatMap (validateNet ecSet) nets ++
            concatMap (validateElmCustom ecSet ("in extra types")) extraTLst
    in
        if not (validateType startNet) then
            ["invalid name for starting net `"++T.unpack startNet++"`"] ++ validations
        else 
            validations