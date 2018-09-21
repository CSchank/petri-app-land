{-# LANGUAGE QuasiQuotes #-}

module ServerTemplate.Decode where

import Text.RawString.QQ
import Data.Text as T

decodeHs :: T.Text
decodeHs = T.pack $ [r|module Utils.Decode where

import Data.Char (ord,chr)

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

infixl 0 |>

(<|) :: (a -> b) -> a -> b
(<|) f x = f x

infixl 0 <|

data Result error value = 
      Err error 
    | Ok value

type List a = [a]

rMap :: (a -> value) -> Result x a -> Result x value
rMap fn ra =
    case ra of 
        Err x -> Err x
        Ok a -> Ok $ fn a


rMap2 :: (a -> b -> value) -> Result x a -> Result x b -> Result x value
rMap2 fn ra rb =
    case ra of 
        Err x -> Err x
        Ok a -> 
            case rb of 
                Err x -> Err x
                Ok b -> Ok $ fn a b

rMap3 :: (a -> b -> c -> value) -> Result x a -> Result x b -> Result x c -> Result x value
rMap3 fn ra rb rc =
    case ra of 
        Err x -> Err x
        Ok a -> 
            case rb of 
                Err x -> Err x
                Ok b ->
                    case rc of 
                        Err x -> Err x
                        Ok c -> Ok $ fn a b c

rMap4 :: (a -> b -> c -> d -> value) -> Result x a -> Result x b -> Result x c -> Result x d -> Result x value
rMap4 fn ra rb rc rd =
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
                                Ok d -> Ok $ fn a b c d

rMap5 :: (a -> b -> c -> d -> e -> value) -> Result x a -> Result x b -> Result x c -> Result x d -> Result x e -> Result x value
rMap5 fn ra rb rc rd re =
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
                                        Ok e -> Ok $ fn a b c d e

rMap6 :: (a -> b -> c -> d -> e -> f -> value) -> Result x a -> Result x b -> Result x c -> Result x d -> Result x e -> Result x f -> Result x value
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
                                                Ok f -> Ok $ fn a b c d e f

rMap7 :: (a -> b -> c -> d -> e -> f -> g -> value) -> Result x a -> Result x b -> Result x c -> Result x d -> Result x e -> Result x f -> Result x g -> Result x value
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
                                                        Ok g -> Ok $ fn a b c d e f g

rMap8 :: (a -> b -> c -> d -> e -> f -> g -> h -> value)
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
                                                                Ok h -> Ok $ fn a b c d e f g h

rMap9 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> value)
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
                                                                        Ok i -> Ok $ fn a b c d e f g h i

rMap10 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> value)
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
                                                                                Ok j -> Ok $ fn a b c d e f g h i j

clamp :: Ord number => number -> number -> number -> number
clamp a b x = min b (max a x)

randThen :: (a -> Result x b) -> Result x a -> Result x b
randThen callback result =
    case result of
      Ok value ->
        callback value

      Err msg ->
        Err msg

rwithDefault :: a -> Result x a -> a
rwithDefault def result =
  case result of
    Ok a ->
        a

    Err _ ->
        def

sConcat :: [String] -> String
sConcat = sConcat


encodeInt :: Int -> Int -> Int -> String
encodeInt low high n =
    let
        encodeInt_ ::  Int -> String
        encodeInt_ nn =
            let 
                b = 64
                r = nn `mod` b
                m = nn `div` b
            in    
                if nn == 0 then ""
                else (chr <| r + 48) : encodeInt_ m
    in
        encodeInt_ (clamp low high n)

decodeInt :: Int -> Int -> String -> Result String Int
decodeInt low high s =
    let 
        decodeInt_ m s_ = case s_ of
                            f:rest -> (ord f - 48) * m + decodeInt_ (m*64) rest
                            []      -> 0
        n = decodeInt_ 1 s
    in
        if n >= low && n <= high then  Ok <| n
        else                           Err <| "Could not decode " ++ show n ++ " as it is outside the range [" ++ show low ++ "," ++ show high ++ "]."

decodeList :: List String -> ((Result String a, List String) -> (Result String a, List String)) -> (Result String (List a), List String)
decodeList ls decodeFn =
    let 
        aR :: Result String a -> Result String (List a) -> Result String (List a)
        aR aRes laRes =
            rMap2 (\a la -> la ++ [a]) aRes laRes
        n =
            rwithDefault 0 <| case ls of 
                nTxt:rest -> decodeInt 0 16777215 nTxt
                []           -> Err "Could not decode number of items in list."

        decodeList_ :: Int -> (Result String (List a),[String]) -> (Result String (List a),[String])
        decodeList_ _ (resL, mainLs) = 
            let 
                (newRes, newLs) = decodeFn (Err "", mainLs)
            in
                (aR newRes resL, newLs)
    in
        foldl decodeList_ ([], ls) [1..n]

|]