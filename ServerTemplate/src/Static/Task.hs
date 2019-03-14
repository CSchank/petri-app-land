{-# LANGUAGE ExistentialQuantification #-}

module Static.Task where

import Static.ServerTypes
--import Static.Cmd
import Utils.Utils
import Control.Monad (foldM)
import Data.Maybe (fromJust)
import Data.TMap as TM
import Static.Cmd as Cmd (Cmd(..))
import Static.Result as Result (Result(..))

data Task err ok = 
      Task (IO (Result err ok))
    | forall x. TaskOnError (x -> Task err ok) (Task x ok)
    | forall state. Plugin state => StateTask (state -> IO (Result err ok))
    | forall a. TaskAndThen (a -> Task err ok) (Task err a)

data Never

evalTask :: TM.TMap -> Task err ok -> IO (Result err ok)
evalTask ps (Task t) = t
evalTask ps (TaskOnError xToTOk tOk) = do
    tR <- evalTask ps tOk
    case tR of
        Ok a ->
            return $ Ok a
        Err x ->
            evalTask ps (xToTOk x)
evalTask ps (StateTask toMsg) = 
    toMsg (safeFromJust "Tried to load a plugin that's not installed." $ TM.lookup ps)
evalTask ps (TaskAndThen aToTb ta) = do
    taResult <- evalTask ps ta
    case taResult of
        Ok a ->
            evalTask ps $ aToTb a
        Err x ->
            return $ Err x

perform :: (a -> msg) -> Task Never a -> Cmd msg
perform toMsg task = TaskCmd $ \tm -> do
    (Ok taskResult) <- evalTask tm task
    return $ toMsg taskResult

attempt :: (Result x a -> msg) -> Task x a -> Cmd msg
attempt toMsg task = TaskCmd $ \tm -> do
    taskResult <- evalTask tm task
    return $ toMsg taskResult

andThen :: (a -> Task x b) -> Task x a -> Task x b
andThen = TaskAndThen
            
succeed :: a -> Task x a
succeed a = Task $ return $ Ok a

fail :: x -> Task x a
fail x = Task $ return $ Err x

map :: (a -> b) -> Task x a -> Task x b
map f taskA = 
    taskA |>
        andThen (succeed . f)

map2 :: (a -> b -> result) -> Task x a -> Task x b -> Task x result
map2 f taskA taskB = 
    taskA 
        |> andThen (\a -> taskB 
        |> andThen (\b -> succeed (f a b)))

map3 :: (a -> b -> c -> result) -> Task x a -> Task x b -> Task x c -> Task x result
map3 f taskA taskB taskC = 
    taskA 
        |> andThen (\a -> taskB 
        |> andThen (\b -> taskC
        |> andThen (\c -> succeed (f a b c))))

map4 :: (a -> b -> c -> d -> result) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x result
map4 f taskA taskB taskC taskD = 
    taskA 
        |> andThen (\a -> taskB 
        |> andThen (\b -> taskC
        |> andThen (\c -> taskD
        |> andThen (\d -> succeed (f a b c d)))))


map5 :: (a -> b -> c -> d -> e -> result) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x e -> Task x result
map5 f taskA taskB taskC taskD taskE = 
    taskA 
        |> andThen (\a -> taskB 
        |> andThen (\b -> taskC
        |> andThen (\c -> taskD
        |> andThen (\d -> taskE
        |> andThen (\e -> succeed (f a b c d e))))))

onError :: (x -> Task y a) -> Task x a -> Task y a
onError =
    TaskOnError

mapError :: (x -> y) -> Task x a -> Task y a
mapError xToy tX =
    tX
        |> onError (Static.Task.fail . xToy)

sequence :: [Task x a] -> Task x [a]
sequence tasks =
    foldr (map2 (:)) (succeed []) tasks


foldl :: (a -> b -> a) -> a -> [Task x b] -> Task x a
foldl fn initA tasks =
    Static.Task.sequence tasks 
        |> Static.Task.map (Prelude.foldl fn initA)