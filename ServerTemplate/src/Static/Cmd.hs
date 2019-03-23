{-# LANGUAGE ExistentialQuantification #-}

module Static.Cmd where

{-foldl :: (a -> b -> a) -> a -> [Cmd b] -> (b -> msg) -> Cmd msg
foldl fn initA lstCmdB toMsg = Cmd $ do-}

import Static.ServerTypes

import           Control.Concurrent.STM         (TQueue, atomically, readTQueue,
                                                 writeTQueue, STM, newTQueue)
import Control.Concurrent.Async (mapConcurrently,mapConcurrently_, Async, wait, async)
import Static.Types
import Data.TMap as TM
import Static.Result as Result (Result)
import Utils.Utils (safeFromJust)



data Cmd msg =
    Cmd (IO msg)
  | CmdBatch [Cmd msg]
  | forall a b. CmdFold (a -> b -> a) a [Cmd b] (a -> IO msg)        --fold over many commands and combine their results
  | forall state. Plugin state => StateCmd (state -> IO msg)
  | TaskCmd (TMap -> IO msg)
  | forall a. CmdAndThen (a -> Cmd msg) (Cmd a)                   -- perform one command using the results of another


fold :: (a -> b -> a) -> a -> [Cmd b] -> (a -> msg) -> Cmd msg
fold fn a cmdBs toMsg = CmdFold fn a cmdBs (return . toMsg)

none :: Cmd msg
none = CmdBatch []

andThen :: (a -> Cmd msg) -> Cmd a -> Cmd msg
andThen = CmdAndThen

map :: (a -> b) -> Cmd a -> Cmd b
map f ca =
    case ca of 
        Cmd msg -> Cmd (fmap f msg)
        CmdBatch cmds -> CmdBatch $ Prelude.map (Static.Cmd.map f) cmds
        StateCmd toMsg -> StateCmd (fmap f . toMsg)
        CmdFold fn a cmdBs toMsg -> CmdFold fn a cmdBs (fmap f . toMsg)
        TaskCmd toMsg -> TaskCmd (fmap f . toMsg)
        CmdAndThen toCmdMsg cmdA -> CmdAndThen (Static.Cmd.map f . toCmdMsg) cmdA

eval :: NetState player -> Cmd t -> IO [Async t]
eval ns cmd = do
    case cmd of
        Cmd msg -> sequence [async msg]
        CmdBatch cmds -> do
            -- these have no order to them
            results <- mapM (eval ns) cmds
            return $ concat results
        CmdFold fn a cmdBs toMsg -> do
            results <- mapM (eval ns) cmdBs -- launch all of the commands asyncronously
            sequence [async . toMsg . foldl fn a =<< mapM wait (concat results)] -- foldl over the results in order
        StateCmd toMsg -> do
            sequence [async $ toMsg ((safeFromJust "plugin not installed!") $ TM.lookup $ pluginStates ns)]
        TaskCmd toMsg -> do
            sequence [async $ toMsg $ pluginStates ns]
        CmdAndThen toCmdMsg cmdA -> do
            result <- eval ns cmdA
            cmds <- mapConcurrently (\a -> wait a >>= eval ns . toCmdMsg) result
            return $ concat cmds

process :: Cmd NetTransition -> TQueue CentralMessage -> NetState player -> IO ()
process cmd centralMsgQ ns = do
    results <- eval ns cmd
    -- concurrently map over all commands, they could arrive in any order at this point
    mapConcurrently_ (\a -> wait a >>= atomically . writeTQueue centralMsgQ . ReceivedMessage Nothing) results
    {-case cmd of
        Cmd msg -> do
            result <- eval ns cmd
            atomically $ writeTQueue centralMsgQ $ ReceivedMessage Nothing (head result)
        StateCmd toMsg -> do
            result <- eval ns cmd
            atomically $ writeTQueue centralMsgQ $ ReceivedMessage Nothing (head result)
        CmdBatch cmds ->
            mapM_ (\c -> forkIO $ processCmd c centralMsgQ ns) cmds
        CmdFold fn a cmdBs toMsg -> do
            evals <- sequence $ map (eval ns) cmdBs
            result <- toMsg $ foldl fn a (concat evals)
            atomically $ writeTQueue centralMsgQ $ ReceivedMessage Nothing result-}