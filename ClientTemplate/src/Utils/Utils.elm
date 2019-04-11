module Utils.Utils exposing(..)

import Char     exposing    (toCode, fromCode)
import String   exposing    (toList)
import Html     exposing    (text)
import Tuple    exposing    (second)
import Result
import String
import Dict exposing (Dict)
import Task

tConcat = String.concat

randThen = Result.andThen

rMap = Result.map
rMap1 = Result.map
rMap2 = Result.map2
rMap3 = Result.map3
rMap4 = Result.map4
rMap5 = Result.map5
        
rMap6 : (a -> b -> c -> d -> e -> f -> value) -> Result x a -> Result x b -> Result x c -> Result x d -> Result x e -> Result x f -> Result x value
rMap6 fn ra rb rc rd re rf =
    case ra of 
        Err x -> Err x
        Ok a -> 
            case rb of 
                Err x -> Err x
                Ok b ->
                    case rc of 
                        Err x -> Err x
                        Ok c -> 
                            case rd of 
                                Err x -> Err x
                                Ok d -> 
                                    case re of 
                                        Err x -> Err x
                                        Ok e ->
                                            case rf of 
                                                Err x -> Err x
                                                Ok f -> Ok <| fn a b c d e f

rMap7 : (a -> b -> c -> d -> e -> f -> g -> value) -> Result x a -> Result x b -> Result x c -> Result x d -> Result x e -> Result x f -> Result x g -> Result x value
rMap7 fn ra rb rc rd re rf rg =
    case ra of 
        Err x -> Err x
        Ok a -> 
            case rb of 
                Err x -> Err x
                Ok b ->
                    case rc of 
                        Err x -> Err x
                        Ok c -> 
                            case rd of 
                                Err x -> Err x
                                Ok d -> 
                                    case re of 
                                        Err x -> Err x
                                        Ok e ->
                                            case rf of 
                                                Err x -> Err x
                                                Ok f -> 
                                                    case rg of 
                                                        Err x -> Err x
                                                        Ok g -> Ok <| fn a b c d e f g

rMap8 : (a -> b -> c -> d -> e -> f -> g -> h -> value)
      -> Result x a
      -> Result x b
      -> Result x c
      -> Result x d
      -> Result x e
      -> Result x f
      -> Result x g
      -> Result x h 
      -> Result x value
rMap8 fn ra rb rc rd re rf rg rh =
    case ra of 
        Err x -> Err x
        Ok a -> 
            case rb of 
                Err x -> Err x
                Ok b ->
                    case rc of 
                        Err x -> Err x
                        Ok c -> 
                            case rd of 
                                Err x -> Err x
                                Ok d -> 
                                    case re of 
                                        Err x -> Err x
                                        Ok e ->
                                            case rf of 
                                                Err x -> Err x
                                                Ok f -> 
                                                    case rg of 
                                                        Err x -> Err x
                                                        Ok g ->
                                                            case rh of 
                                                                Err x -> Err x
                                                                Ok h -> Ok <| fn a b c d e f g h

rMap9 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> value)
      -> Result x a
      -> Result x b
      -> Result x c
      -> Result x d
      -> Result x e
      -> Result x f
      -> Result x g
      -> Result x h
      -> Result x i
      -> Result x value
rMap9 fn ra rb rc rd re rf rg rh ri =
    case ra of 
        Err x -> Err x
        Ok a -> 
            case rb of 
                Err x -> Err x
                Ok b ->
                    case rc of 
                        Err x -> Err x
                        Ok c -> 
                            case rd of 
                                Err x -> Err x
                                Ok d -> 
                                    case re of 
                                        Err x -> Err x
                                        Ok e ->
                                            case rf of 
                                                Err x -> Err x
                                                Ok f -> 
                                                    case rg of 
                                                        Err x -> Err x
                                                        Ok g ->
                                                            case rh of 
                                                                Err x -> Err x
                                                                Ok h ->
                                                                    case ri of 
                                                                        Err x -> Err x
                                                                        Ok i -> Ok <| fn a b c d e f g h i

rMap10 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> value)
      -> Result x a
      -> Result x b
      -> Result x c
      -> Result x d
      -> Result x e
      -> Result x f
      -> Result x g
      -> Result x h
      -> Result x i
      -> Result x j
      -> Result x value
rMap10 fn ra rb rc rd re rf rg rh ri rj =
    case ra of 
        Err x -> Err x
        Ok a -> 
            case rb of 
                Err x -> Err x
                Ok b ->
                    case rc of 
                        Err x -> Err x
                        Ok c -> 
                            case rd of 
                                Err x -> Err x
                                Ok d -> 
                                    case re of 
                                        Err x -> Err x
                                        Ok e ->
                                            case rf of 
                                                Err x -> Err x
                                                Ok f -> 
                                                    case rg of 
                                                        Err x -> Err x
                                                        Ok g ->
                                                            case rh of 
                                                                Err x -> Err x
                                                                Ok h ->
                                                                    case ri of 
                                                                        Err x -> Err x
                                                                        Ok i ->
                                                                            case rj of 
                                                                                Err x -> Err x
                                                                                Ok j -> Ok <| fn a b c d e f g h i j

rMap11 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> value)
      -> Result x a
      -> Result x b
      -> Result x c
      -> Result x d
      -> Result x e
      -> Result x f
      -> Result x g
      -> Result x h
      -> Result x i
      -> Result x j
      -> Result x k    
      -> Result x value
rMap11 fn r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 =
    r1 |> Result.andThen (\ok1 -> r2
       |> Result.andThen (\ok2 -> r3
       |> Result.andThen (\ok3 -> r4
       |> Result.andThen (\ok4 -> r5
       |> Result.andThen (\ok5 -> r6
       |> Result.andThen (\ok6 -> r7
       |> Result.andThen (\ok7 -> r8
       |> Result.andThen (\ok8 -> r9
       |> Result.andThen (\ok9 -> r10
       |> Result.andThen (\ok10 -> r11
       |> Result.andThen (\ok11 -> Ok <| fn ok1 ok2 ok3 ok4 ok5 ok6 ok7 ok8 ok9 ok10 ok11)))))))))))

rMap12 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> value)
      -> Result x a
      -> Result x b
      -> Result x c
      -> Result x d
      -> Result x e
      -> Result x f
      -> Result x g
      -> Result x h
      -> Result x i
      -> Result x j
      -> Result x k
      -> Result x l   
      -> Result x value
rMap12 fn r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 =
    r1 |> Result.andThen (\ok1 -> r2
       |> Result.andThen (\ok2 -> r3
       |> Result.andThen (\ok3 -> r4
       |> Result.andThen (\ok4 -> r5
       |> Result.andThen (\ok5 -> r6
       |> Result.andThen (\ok6 -> r7
       |> Result.andThen (\ok7 -> r8
       |> Result.andThen (\ok8 -> r9
       |> Result.andThen (\ok9 -> r10
       |> Result.andThen (\ok10 -> r11
       |> Result.andThen (\ok11 -> r12
       |> Result.andThen (\ok12 -> Ok <| fn ok1 ok2 ok3 ok4 ok5 ok6 ok7 ok8 ok9 ok10 ok11 ok12))))))))))))
        
rMap13 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> value)
      -> Result x a
      -> Result x b
      -> Result x c
      -> Result x d
      -> Result x e
      -> Result x f
      -> Result x g
      -> Result x h
      -> Result x i
      -> Result x j
      -> Result x k
      -> Result x l
      -> Result x m       
      -> Result x value
rMap13 fn r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 =
    r1 |> Result.andThen (\ok1 -> r2
       |> Result.andThen (\ok2 -> r3
       |> Result.andThen (\ok3 -> r4
       |> Result.andThen (\ok4 -> r5
       |> Result.andThen (\ok5 -> r6
       |> Result.andThen (\ok6 -> r7
       |> Result.andThen (\ok7 -> r8
       |> Result.andThen (\ok8 -> r9
       |> Result.andThen (\ok9 -> r10
       |> Result.andThen (\ok10 -> r11
       |> Result.andThen (\ok11 -> r12
       |> Result.andThen (\ok12 -> r13
       |> Result.andThen (\ok13 -> Ok <| fn ok1 ok2 ok3 ok4 ok5 ok6 ok7 ok8 ok9 ok10 ok11 ok12 ok13)))))))))))))

rMap14 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> value)
      -> Result x a
      -> Result x b
      -> Result x c
      -> Result x d
      -> Result x e
      -> Result x f
      -> Result x g
      -> Result x h
      -> Result x i
      -> Result x j
      -> Result x k
      -> Result x l
      -> Result x m
      -> Result x n
      -> Result x value
rMap14 fn r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 =
    r1 |> Result.andThen (\ok1 -> r2
       |> Result.andThen (\ok2 -> r3
       |> Result.andThen (\ok3 -> r4
       |> Result.andThen (\ok4 -> r5
       |> Result.andThen (\ok5 -> r6
       |> Result.andThen (\ok6 -> r7
       |> Result.andThen (\ok7 -> r8
       |> Result.andThen (\ok8 -> r9
       |> Result.andThen (\ok9 -> r10
       |> Result.andThen (\ok10 -> r11
       |> Result.andThen (\ok11 -> r12
       |> Result.andThen (\ok12 -> r13
       |> Result.andThen (\ok13 -> r14                              
       |> Result.andThen (\ok14 -> Ok <| fn ok1 ok2 ok3 ok4 ok5 ok6 ok7 ok8 ok9 ok10 ok11 ok12 ok13 ok14))))))))))))))

rMap15 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> value)
      -> Result x a
      -> Result x b
      -> Result x c
      -> Result x d
      -> Result x e
      -> Result x f
      -> Result x g
      -> Result x h
      -> Result x i
      -> Result x j
      -> Result x k
      -> Result x l
      -> Result x m
      -> Result x n
      -> Result x o
      -> Result x value
rMap15 fn r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 =
    r1 |> Result.andThen (\ok1 -> r2
       |> Result.andThen (\ok2 -> r3
       |> Result.andThen (\ok3 -> r4
       |> Result.andThen (\ok4 -> r5
       |> Result.andThen (\ok5 -> r6
       |> Result.andThen (\ok6 -> r7
       |> Result.andThen (\ok7 -> r8
       |> Result.andThen (\ok8 -> r9
       |> Result.andThen (\ok9 -> r10
       |> Result.andThen (\ok10 -> r11
       |> Result.andThen (\ok11 -> r12
       |> Result.andThen (\ok12 -> r13
       |> Result.andThen (\ok13 -> r14
       |> Result.andThen (\ok14 -> r15                   
       |> Result.andThen (\ok15 -> Ok <| fn ok1 ok2 ok3 ok4 ok5 ok6 ok7 ok8 ok9 ok10 ok11 ok12 ok13 ok14 ok15)))))))))))))))
                              
encodeInt : Int -> Int -> Int -> String
encodeInt low high n =
    let
        encodeInt_ :  Int -> String
        encodeInt_ nn =
            let 
                b = 64
                r = modBy b nn
                m = nn // b
            in    
                if nn < 64 then (String.fromChar <| fromCode <| r + 48)
                else (String.fromChar <| fromCode <| r + 48) ++ encodeInt_ m
    in
        encodeInt_ (clamp low high n - low)

decodeInt : Int -> Int -> String -> Result String Int
decodeInt low high s =
    let 
        decodeInt_ m s_ = case s_ of
                            f::rest -> (toCode f - 48) * m + decodeInt_ (m*64) rest
                            []      -> 0
        n = (decodeInt_ 1 <| toList s) + low
    in
        if n >= low && n <= high then  Ok <| n
        else                           Err <| "Could not decode " ++ String.fromInt n ++ " as it is outside the range [" ++ String.fromInt low ++ "," ++ String.fromInt high ++ "]."

decodeList : List String -> ((Result String a, List String) -> (Result String a, List String)) -> (Result String (List a), List String)
decodeList ls decodeFn =
    let 
        aR : Result String a -> Result String (List a) -> Result String (List a)
        aR aRes laRes =
            Result.map2 (\a la -> la ++ [a]) aRes laRes
        n =
            Result.withDefault 0 <| case ls of 
                nTxt :: rest -> decodeInt 0 16777215 nTxt
                []           -> Err "Could not decode number of items in list."

        decodeList_ n_ (resL, mainLs) = 
            let 
                (newRes, newLs) = decodeFn (Err "", mainLs)
            in
                (aR newRes resL, newLs)
    in
        List.foldl decodeList_ (Ok [], List.drop 1 ls) (List.range 1 n)
        
decodeMaybe : List String -> ((Result String a, List String) -> (Result String a, List String)) -> (Result String (Maybe a), List String)
decodeMaybe ls decodeFn =
    case ls of
        ("J"::rest) -> 
            let
                (newRes, newLs) = decodeFn (Err "", rest)
            in
                (Result.map Just newRes,newLs)
        ("N"::rest) ->
            (Ok Nothing, rest)
        _ -> (Err "Ran out of items or error while decoding a Maybe.",[])

decodeBool : List String -> (Result String Bool, List String)
decodeBool ls =
    case ls of
        ("T"::rest) -> (Ok True, rest)
        ("F"::rest) -> (Ok False, rest)
        _ -> (Err "error decoding boolean value",[])

decodeString : List String -> (Result String String, List String)
decodeString ls =
    case ls of
        fst::rest -> (Ok fst, rest)
        _ -> (Err "Error decoding string value",[])

decodeDict : (Result String (List (comparable,b)), List String) -> (Result String (Dict comparable b), List String)
decodeDict (res,lst) =
    (rMap Dict.fromList res, lst)


decodeResult : 
    ((Result String x, List String) -> (Result String x, List String)) ->
    ((Result String a, List String) -> (Result String a, List String)) ->
    List String ->
    (Result String (Result x a), List String)
decodeResult decodeErr decodeOk lst =
    case lst of
        "Ok"::rest ->
            let
                (ok,newrest) =
                    decodeOk (Err "",rest)
            in
                (Result.map Ok ok, newrest)
        "Err"::rest ->
            let
                (err,newrest) =
                    decodeErr (Err "",rest)
            in
                (Result.map Err err, newrest)
        [] ->
            (Err "Ran out of strings while decoding a Result.", [])
        a ->
            (Err <| "Error while decoding Result. Leftover strings: [" ++ String.join "," a ++ "]", a)

lLength = List.length

pFst = Tuple.first

lFoldl : (a -> b -> b) -> b -> List a -> b
lFoldl = List.foldl

lRange = List.range

newMsg : msg -> Cmd msg
newMsg msg =
    Task.perform identity (Task.succeed msg)
