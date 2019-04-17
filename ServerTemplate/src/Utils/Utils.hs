{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Utils.Utils where

import Data.Char (ord,chr)
import qualified Data.Text as T
import Static.ServerTypes
import Static.Types
import           Control.Concurrent.STM         (TQueue, atomically, readTQueue,
                                                 writeTQueue, STM, newTQueue)
import qualified Data.Map.Strict as Dict
import Static.Dict
import qualified Data.TMap as TM
import Data.Maybe (fromJust,isJust)
import qualified Data.IntMap.Strict as IM'
import Control.Concurrent (forkIO)
import qualified Static.Result as Result

import qualified Data.Time.Clock.POSIX         as Time


(|>) :: a -> (a -> b) -> b
(|>) x f = f x

infixl 0 |>

(<|) :: (a -> b) -> a -> b
(<|) f x = f x

infixr 0 <|

toFloat :: Integral a => a -> Double
toFloat = fromIntegral

clamp :: Ord number => number -> number -> number -> number
clamp a b x = min b (max a x)

tConcat :: [T.Text] -> T.Text
tConcat = T.concat

sConcat :: [String] -> String
sConcat = concat


encodeInt :: Int -> Int -> Int -> T.Text
encodeInt low high n =
    let
        encodeInt_ :: Int -> [Char]
        encodeInt_ nn =
            let 
                b = 64
                r = nn `mod` b
                m = nn `div` b
            in    
                if nn < 64 then [chr <| r + 48]
                else (chr <| r + 48) : encodeInt_ m
    in
        T.pack $ encodeInt_ (clamp low high n - low)

decodeInt :: Int -> Int -> T.Text -> Result.Result T.Text Int
decodeInt low high s =
    let 
        decodeInt_ m s_ = case s_ of
                            f:rest -> (ord f - 48) * m + decodeInt_ (m*64) rest
                            []      -> 0
        n = (decodeInt_ 1 $ T.unpack s) + low
    in
        if n >= low && n <= high then  Result.Ok <| n
        else                           Result.Err <| T.concat ["Could not decode ", T.pack $ show n, " as it is outside the range [", T.pack $ show low, ",", T.pack $ show high, "]."]

decodeList :: forall a . [T.Text] -> ((Result.Result T.Text a, [T.Text]) -> (Result.Result T.Text a, [T.Text])) -> (Result.Result T.Text [a], [T.Text])
decodeList ls decodeFn =
    let 
        aR :: Result.Result T.Text a -> Result.Result T.Text [a] -> Result.Result T.Text [a]
        aR aRes laRes =
            Result.map2 (\a la -> la ++ [a]) aRes laRes
        n =
            Result.withDefault 0 <| case ls of 
                nTxt:rest -> decodeInt 0 16777215 nTxt
                []           -> Result.Err "Could not decode number of items in list."

        decodeList' :: (Result.Result T.Text [a],[T.Text]) -> Int -> (Result.Result T.Text [a],[T.Text])
        decodeList' (resL, mainLs) _ = 
            let 
                (newRes, newLs) = decodeFn (Result.Err "", mainLs)
            in
                (aR newRes resL, newLs)
    in
        foldl decodeList' (Result.Ok [], drop 1 ls) [1..n]

decodeBool :: [T.Text] -> (Result.Result T.Text Bool, [T.Text])
decodeBool ls =
    case ls of
        "T":rest -> (Result.Ok True, rest)
        "F":rest -> (Result.Ok False, rest)
        _ -> (Result.Err "Error decoding boolean value",[])

decodeString :: [T.Text] -> (Result.Result T.Text String, [T.Text])
decodeString ls =
    case ls of
        fst:rest -> (Result.Ok $ T.unpack fst, rest)
        _ -> (Result.Err "Error decoding string value",[])

decodeMaybe :: [T.Text] -> ((Result.Result T.Text a, [T.Text]) -> (Result.Result T.Text a, [T.Text])) -> (Result.Result T.Text (Maybe a), [T.Text])
decodeMaybe ls decodeFn =
    case ls of
        ("J":rest) -> 
            let
                (newRes, newLs) = decodeFn (Result.Err "", rest)
            in
                (Result.map Just newRes,newLs)
        ("N":rest) ->
            (Result.Ok Nothing, rest)
        _ -> (Result.Err "Ran out of items or error while decoding a Maybe.",[])

decodeDict :: Ord a => (Result.Result T.Text [(a,b)], [T.Text]) -> (Result.Result T.Text (Dict a b), [T.Text])
decodeDict (res,lst) =
    (Result.map Dict.fromList res, lst)
        
giveReceivingQueue :: TQueue a -> (TQueue b -> a) -> STM (TQueue b)
giveReceivingQueue commandQueue signalConstructor = do
    channel <- newTQueue
    writeTQueue commandQueue (signalConstructor channel)
    return channel

lLength :: Foldable t => t a -> Int
lLength = length

pFst = fst

lFoldl :: (a -> b -> b) -> b -> [a] -> b
lFoldl f init lst = foldl (flip f) init lst

lRange a b = [a..b]



safeFromJust msg j = 
    if isJust j then
        fromJust j
    else
        error $ "not isJust: " ++ msg

mapSnd :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnd f l =
    map (\(c,a) -> (c,f a)) l

insertList :: [(IM'.Key, a)] -> IM'.IntMap a -> IM'.IntMap a
insertList l im =
    foldl (\m (i,a) -> IM'.insert i a m) im l


getTime :: IO Time.POSIXTime
getTime = Time.getPOSIXTime