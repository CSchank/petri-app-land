module Plugins.Incrementer where
import Control.Concurrent.STM (STM, TQueue, TVar, newTVar, readTVar, writeTVar, atomically, newTQueue, readTQueue, writeTQueue)
import Static.ServerTypes


data Counter = Counter (TVar Int) --anything you need the runtime to keep track of

instance (Plugin Counter) where
    initPlugin = fmap Counter $ atomically $ newTVar 0

doIncrement :: (Int -> msg) -> Cmd msg
doIncrement msgf = StateCmd $ \(Counter counter) -> do
    currN <- atomically $ readTVar counter
    atomically $ writeTVar counter $ currN + 1
    return $ msgf $ currN + 1

doDecrement :: (Int -> msg) -> Cmd msg
doDecrement msgf = StateCmd $ \(Counter counter) -> do
    currN <- atomically $ readTVar counter
    atomically $ writeTVar counter $ currN - 1
    return $ msgf $ currN - 1


getCounter :: (Int -> msg) -> Cmd msg
getCounter msgf = StateCmd $ \(Counter counter) -> do
    currN <- atomically $ readTVar counter
    return $ msgf currN

{-type Msg =
    Increment

increment :: Int -> State CounterState Counter
increment n = do
    (currN, state) <- get
    put (currN + n, if currN + n > 10 then False else True)
    return $ currN + n
-}