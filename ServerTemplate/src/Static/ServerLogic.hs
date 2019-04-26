{-# LANGUAGE OverloadedStrings #-}
module Static.ServerLogic where

import           Control.Concurrent.STM (STM, TQueue, TVar, atomically, newTQueue,
                                         readTQueue, writeTQueue, newTVar, readTVar, 
                                         TMVar, putTMVar
                                         )
import           Control.Monad          (forever,void)
import qualified Data.Map.Strict        as M'
import qualified Data.IntMap.Strict     as IM'
import           Data.Text              (Text)
import           Control.Monad.IO.Class         (liftIO)
import qualified Data.Text              as T
import qualified Network.WebSockets     as WS
import           Control.Concurrent     (forkIO, result)
import           Control.Concurrent.Async       (race_)
import           Control.Exception      (finally)
import qualified Data.Time.Clock.POSIX         as Time


import           qualified Data.Text.IO                   as Tio
import qualified Data.Set               as S
import qualified Data.TMap              as TM
import Data.Maybe (fromJust)

import Static.Types
import Static.ServerTypes
import Static.Encode
import Static.Decode
import Static.Init
import Static.Plugins
import Utils.Utils (safeFromJust)
import Static.Update (update, clientConnect, disconnect)

import Static.Result (Result(..))

newCentralMessageChan :: STM (TQueue CentralMessage)
newCentralMessageChan =
    newTQueue


newClientMessageChan :: STM (TQueue OutgoingClientThreadMessage)
newClientMessageChan =
    newTQueue

processCentralChan :: TQueue CentralMessage -> TMVar () -> IO ()
processCentralChan chan ks =
    -- This loop holds the server state and reads from the CentralMessage
    -- channel.
    let
        loop :: ServerState -> IO ()
        loop state =
            atomically (readTQueue chan) >>= processCentralMessage chan state >>= loop

        initial :: Int -> TM.TMap -> ServerState
        initial t serverState =
            ServerState
            { clients = IM'.empty
            , serverState = serverState
            , nextClientId = 0
            , startTime = t
            , killSwitch = ks
            }
    in do
        t <- Time.getPOSIXTime
        init <- Static.Init.init
        loop $ initial (round $ t * 1000) (TM.insert init TM.empty)


processCentralMessage :: (TQueue CentralMessage) -> ServerState -> CentralMessage -> IO ServerState
processCentralMessage centralMessageChan state (NewUser clientMessageChan conn) = do
    -- A new Client has signed on. We need to add the following to the server
    -- state:
    -- (1) Add their ClientThreadMessage channel for broadcasting future updates.
    -- (2) Add their username as a key to the scoring Dict with a fresh value.

    let newClientId = (nextClientId state)

    tvar <- atomically $ newTVar Static.Init.initNet   --stores the user's current net (to know how to decode the message)

    Prelude.putStrLn $ "Processing new client with ID " ++ show newClientId
    Prelude.putStrLn $ "Current clients: " ++ show (IM'.keys $ clients state)

    -- Add the new Client to the server state.
    let nextClients = IM'.insert newClientId (Client tvar clientMessageChan 0) (clients state)

    -- Run two threads and terminate when either dies.
    -- (1) Process the new message channel responsible for this Client.
    -- (2) Read the WebSocket connection and pass it on to the central
    --     message channel.
    forkIO $ finally (race_ (processOutgoingMsg conn clientMessageChan) $ forever (do
        msg <- WS.receiveData conn
        netToDecode <- atomically $ readTVar tvar --read which decoder to use
        Prelude.putStrLn $ "Received " ++ T.unpack msg ++ " from clientID " ++ show newClientId
        parseIncomingMsg newClientId netToDecode centralMessageChan msg
        ))  -- We don't care about the results of `race`
        -- when the connection closes, catch the exception or disconnect and inform our runtime
        (atomically $ writeTQueue centralMessageChan $ UserConnectionLost newClientId)

    Prelude.putStrLn $ "Client with ID " ++ show newClientId ++ " connected successfully!"

    -- inform the user's app that the client has connected
    t <- Time.getPOSIXTime
    let tld = TopLevelData { serverStartTime = startTime state
                           , currentTime = round $ t * 1000
                           }
    let nextState = clientConnect tld newClientId $ state { clients = nextClients, nextClientId = newClientId + 1 }
    -- Provide the new state back to the loop.
    return nextState

processCentralMessage centralMessageChan state (ReceivedMessage mClientID incomingMsg) = 
    let
        connectedClients = clients state

        sendMessages :: [(ClientID, NetOutgoingMessage)] -> IO ()
        sendMessages msgs =
            mapM_ sendToID msgs
        
        sendToID :: (ClientID,NetOutgoingMessage) -> IO ()
        sendToID (clientID, cm) =
            case IM'.lookup clientID connectedClients of
                Just (Client decodeChannel chan netID) -> atomically $ writeTQueue chan $ SendMessage (encodeOutgoingMessage cm)
                Nothing -> Prelude.putStrLn $ "Unable to send message to client " ++ show mClientID ++ " because that client doesn't exist or is logged out."    

    in do
    t <- Time.getPOSIXTime

    let tld = TopLevelData { serverStartTime = startTime state
                           , currentTime = round $ t * 1000
                           }
        (nextState, outgoingMsgs, mCmd) = update tld mClientID incomingMsg state

    sendMessages outgoingMsgs
    forkIO $ processCmd centralMessageChan mCmd incomingMsg nextState

    return nextState

processCentralMessage centralMessageChan state (UserConnectionLost clientID) = 
    let
        connectedClients = clients state
        (Client nmTvar clientQueue netID) = (safeFromJust "userConnectionLost") $ IM'.lookup clientID $ clients state

    in do
    Prelude.putStrLn $ "Client " ++ show clientID ++ " lost connection."
    netModel <- atomically $ readTVar nmTvar

    t <- Time.getPOSIXTime

    -- inform the user's app that the client has disconnected

    let tld = TopLevelData { serverStartTime = startTime state
                           , currentTime = round $ t * 1000
                           }

    disconnect tld clientID netModel state


processCentralMessage centralMessageChan state KillMessageReceived = 
    do
        putStrLn "Preparing to stop server..."
        teardown (serverState state)
        atomically $ putTMVar (killSwitch state) ()
        return state
{-
--get current state of central thread
processCentralMessage centralMessageChan state (GetCurrentState queue) = do
    atomically $ writeTQueue queue state
    return state

--set current state of user server
processCentralMessage centralMessageChan state (SetInternalState newInternalState) = do
    return $ state { internalServerState = newInternalState }

--set current state of user server
processCentralMessage centralMessageChan state ResetClients = do
    let connectedClients = clients state
    atomically $ mapM_ (\(Client chan) -> writeTQueue chan ResetState) $ IM'.elems connectedClients
    return state
-}
--a new message has been received from a client. Process it and inform the central message about it.
parseIncomingMsg :: ClientID -> NetModel -> TQueue CentralMessage -> Text -> IO ()
parseIncomingMsg clientId netToDecode chan msg = do
    case (decodeIncomingMessage msg netToDecode) of
        Ok msg -> atomically $ writeTQueue chan $ ReceivedMessage (Just clientId) msg
        Err er -> Tio.putStrLn $ T.concat ["Error decoding message from client ", T.pack $ show clientId, ". Failed with error: ", er]--, " and the following still in the deocde buffer:", T.pack $ show l, "."]



processOutgoingMsg :: WS.Connection -> TQueue OutgoingClientThreadMessage -> IO ()
processOutgoingMsg conn chan = forever $ do
    -- This reads a ClientThreadMessage channel forever and passes any messages
    -- it reads to the WebSocket Connection.
    clientMsg{-(SendMessage outgoingMessage)-} <- atomically $ readTQueue chan
    case clientMsg of 
        SendMessage outgoingMessage -> do
            WS.sendTextData conn outgoingMessage
            Tio.putStrLn $ T.concat $ ["Sent: ",  outgoingMessage]
        {-ResetState ->
            WS.sendTextData conn ("resetfadsfjewi" :: T.Text)-}