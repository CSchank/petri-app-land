{-# LANGUAGE OverloadedStrings #-}
module Static.ServerLogic
    ( newCentralMessageChan
    , newClientMessageChan
    , processCentralChan
    , processClienTQueue
    ) where

import           Control.Concurrent.STM (STM, TQueue, TVar, atomically, newTQueue,
                                         readTQueue, writeTQueue, newTVar)
import           Control.Monad          (forever,void)
import qualified Data.Map.Strict        as M'
import qualified Data.IntMap.Strict     as IM'
import           Data.Text              (Text)
import           Control.Monad.IO.Class         (liftIO)
import qualified Data.Text              as T
import qualified Network.WebSockets     as WS
import           Control.Concurrent             (forkIO)
import           Control.Concurrent.Async       (race_)
import           Control.Exception      (finally)
import qualified Data.Time.Clock.POSIX         as Time


import           Data.Text.IO                   as Tio
import qualified Data.Set               as S
import qualified Data.TMap              as TM
import Data.Maybe (fromJust)

import Static.Types
import Static.ServerTypes
import Static.Encode
import Static.Decode
import Static.Init (init)
import Static.Plugins
import Utils.Utils (Result(..))
import Static.Update (update)


newCentralMessageChan :: STM (TQueue CentralMessage)
newCentralMessageChan =
    newTQueue


newClientMessageChan :: STM (TQueue ClientThreadMessage)
newClientMessageChan =
    newTQueue

processCentralChan :: TQueue CentralMessage -> IO ()
processCentralChan chan =
    -- This loop holds the server state and reads from the CentralMessage
    -- channel.
    let
        loop :: ServerState -> IO ()
        loop state =
            atomically (readTQueue chan) >>= processCentralMessage chan state >>= loop

        initial :: Int -> ServerState
        initial t = ServerState
            { clients = IM'.empty
            , internalServerState = TM.insert Static.Init.init TM.empty
            , nextClientId = 0
            , startTime = t
            }
    in do
        t <- Time.getPOSIXTime
        loop $ initial (round $ t * 1000)


processCentralMessage :: (TQueue CentralMessage) -> ServerState -> CentralMessage -> IO ServerState
processCentralMessage centralMessageChan state (NewUser clientMessageChan conn) = do
    -- A new Client has signed on. We need to add the following to the server
    -- state:
    -- (1) Add their ClientThreadMessage channel for broadcasting future updates.
    -- (2) Add their username as a key to the scoring Dict with a fresh value.

    let newClientId = (nextClientId state)

    tvar <- atomically $ newTVar    --stores the user's current net (to know how to decode the message)
    atomically $ writeTVar tvar Init.initNet

    Prelude.putStrLn $ "Processing new client with ID " ++ show newClientId
    Prelude.putStrLn $ "Current clients: " ++ show (IM'.keys $ clients state)

    -- Add the new Client to the server state.
    let nextClients = IM'.insert newClientId (Client tvar clientMessageChan 0) (clients state)

    -- Construct the next state with the new list of Clients and new
    -- Dict of scores.
    let nextState = state { clients = nextClients, nextClientId = newClientId + 1 }

    -- Run two threads and terminate when either dies.
    -- (1) Process the new message channel responsible for this Client.
    -- (2) Read the WebSocket connection and pass it on to the central
    --     message channel.
    forkIO $ finally (race_ (processClienTQueue conn clientMessageChan) $ forever (do
        msg <- WS.receiveData conn
        netToDecode <- atomically $ readTVar tvar --read which decoder to use
        Prelude.putStrLn $ "Received " ++ T.unpack msg ++ " from clientID " ++ show newClientId
        parseIncomingMsg newClientId netToDecode centralMessageChan msg
        ))  -- We don't care about the results of `race`
        -- when the connection closes, catch the exception or disconnect and inform our runtime
        (atomically $ writeTQueue centralMessageChan $ UserConnectionLost newClientId)

    Prelude.putStrLn $ "Client with ID " ++ show newClientId ++ " connected successfully!"

    -- inform the user's app that the client has connected
    atomically $ writeTQueue centralMessageChan $ ReceivedMessage newClientId MClientConnect

    -- Provide the new state back to the loop.
    return nextState

processCentralMessage centralMessageChan state (ReceivedMessage mClientID incomingMsg) = 
    let
        connectedClients = clients state

        ps = pluginState state

        sendMessages :: [(ClientID, NetOutgoingMessage)] -> IO ()
        sendMessages msgs =
            mapM_ sendToID msgs
        
        sendToID :: (ClientID,NetOutgoingMessage) -> IO ()
        sendToID (clientID, cm) =
            case IM'.lookup clientId connectedClients of
                Just (Client decodeChannel chan netID) -> atomically $ writeTQueue chan $ SendMessage (encodeOutgoingMessage cm)
                Nothing -> Prelude.putStrLn $ "Unable to send message to client " ++ show clientId ++ " because that client doesn't exist or is logged out."    
        
        (nextState, outgoingMsgs, cmd) = update mClientID incomingMsg (serverState state)

    in do
    sendMessages outgoingMsgs

    --FIXME: do something with the commands

    return nextState

processCentralMessage centralMessageChan state (UserConnectionLost clientID) = 
    let
        connectedClients = clients state
        (Client nmTvar clientQueue netID) = fromJust $ IM'.lookup clientId

    in do
    Prelude.putStrLn $ "Client " ++ show clientId ++ " lost connection."
    netModel <- atomically $ readTVar nmTvar

    -- inform the user's app that the client has disconnected
    return $ disconnect clientID netModel state
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
        Ok msg -> atomically $ writeTQueue chan $ ReceivedMessage clientId msg
        Err er -> Tio.putStrLn $ T.concat ["Error decoding message from client ", T.pack $ show clientId, ". Failed with error: ", er]--, " and the following still in the deocde buffer:", T.pack $ show l, "."]



processOutgoingMsg :: WS.Connection -> TQueue OutgoingClientThreadMessage -> IO ()
processOutgoingMsg conn chan = forever $ do
    -- This reads a ClientThreadMessage channel forever and passes any messages
    -- it reads to the WebSocket Connection.
    clientMsg{-(SendMessage outgoingMessage)-} <- atomically $ readTQueue chan
    case clientMsg of 
        SendMessage outgoingMessage -> do
            let txtMsg = encodeClientMessage outgoingMessage

            WS.sendTextData conn txtMsg
            Tio.putStrLn $ T.concat $ ["Sent: ",  txtMsg]
        {-ResetState ->
            WS.sendTextData conn ("resetfadsfjewi" :: T.Text)-}