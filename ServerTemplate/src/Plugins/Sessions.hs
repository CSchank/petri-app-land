module Plugins.Sessions where

import Static.ServerTypes
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Control.Concurrent.STM (newTVar, TVar, atomically, readTVar, writeTVar)
import Static.Task (Task)
import Data.Word (Word32)
import Static.Result (Result(..))
import Static.Task (Task(..))
import Data.Maybe (fromMaybe)

data Sessions =
    Sessions
        {
            sessions :: TVar (IntMap Client)
        ,   loggedIn :: TVar (IntMap Word32)
        }

instance (Plugin Sessions) where
    initPlugin = do
        putStrLn "Loading sessions plugin...."
        newSessTV <- atomically $ newTVar IM.empty
        newLITV <- atomically $ newTVar IM.empty
        return $ 
            Sessions
                {
                    sessions = newSessTV
                ,   loggedIn = newLITV
                }

newSession :: Client -> Task a ClientID
newSession client = StateTask $ \s -> do
    let sessTV = sessions s
    sess <- atomically $ readTVar sessTV
    let newId = (fromMaybe (-1) $ fmap fst $ IM.lookupMax sess) + 1
        newState = IM.insert (fromIntegral newId) client sess
    atomically $ writeTVar sessTV newState
    return $ Ok newId

removeSession :: ClientID -> Task a Bool
removeSession clientId = StateTask $ \s -> do
    let sessTV = sessions s
    sess <- atomically $ readTVar sessTV
    let exists = IM.member clientId sess
        newState = IM.delete clientId sess
    atomically $ writeTVar sessTV newState
    return $ Ok exists
    