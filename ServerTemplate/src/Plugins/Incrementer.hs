module Plugins.Incrementer where
import Control.Concurrent.STM (STM, TQueue, TVar, newTVar, readTVar, writeTVar, atomically, newTQueue, readTQueue, writeTQueue)
import Static.ServerTypes


data Incrementer = Incrementer (TVar Int) --anything you need the runtime to keep track of

instance (Plugin Incrementer) where
    initPlugin = do
        putStrLn "Initializing incrementer plugin...."
        fmap Incrementer $ atomically $ newTVar 0

doIncrement :: (Int -> msg) -> Cmd msg
doIncrement msgf = StateCmd $ \(Incrementer counter) -> do
    currN <- atomically $ readTVar counter
    atomically $ writeTVar counter $ currN + 1
    return $ msgf $ currN + 1

doDecrement :: (Int -> msg) -> Cmd msg
doDecrement msgf = StateCmd $ \(Incrementer counter) -> do
    currN <- atomically $ readTVar counter
    atomically $ writeTVar counter $ currN - 1
    return $ msgf $ currN - 1


getCounter :: (Int -> msg) -> Cmd msg
getCounter msgf = StateCmd $ \(Incrementer counter) -> do
    currN <- atomically $ readTVar counter
    return $ msgf currN

{-type Msg =
    Increment

increment :: Int -> State CounterState Incrementer
increment n = do
    (currN, state) <- get
    put (currN + n, if currN + n > 10 then False else True)
    return $ currN + n
-}