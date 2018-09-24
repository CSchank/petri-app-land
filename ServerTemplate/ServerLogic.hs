{-# LANGUAGE QuasiQuotes #-}

module ServerTemplate.ServerLogic where

import Text.RawString.QQ
import Data.Text as T

serverLogicHs :: T.Text
serverLogicHs = T.pack $ [r|{-# LANGUAGE OverloadedStrings #-}
module Static.ServerLogic
    ( newCentralMessageChan
    , newClientMessageChan
    , processCentralChan
    , processClientChan
    , CentralMessage(..)
    , ClientMessage(..)
    ) where

import           Control.Concurrent.STM (STM, TChan, atomically, newTChan,
                                         readTChan, writeTChan)
import           Control.Monad          (forever)
import qualified Data.Map.Strict        as M'
import qualified Data.IntMap.Strict     as IM'
import           Data.Text              (Text)
import           Control.Monad.IO.Class         (liftIO)
import qualified Data.Text              as T
import qualified Network.WebSockets     as WS
import           Control.Concurrent             (forkIO)
import           Control.Concurrent.Async       (race_)

import           Data.Text.IO                   as Tio

import Static.Types
import Static.ServerTypes
import Static.Encode
import Static.Decode
import Static.Init (init)
import Utils.Decode (Result(..))
import Static.Update (update)


newCentralMessageChan :: STM (TChan CentralMessage)
newCentralMessageChan =
    newTChan


newClientMessageChan :: STM (TChan ClientMessage)
newClientMessageChan =
    newTChan

processCentralChan :: TChan CentralMessage -> IO ()
processCentralChan chan =
    -- This loop holds the server state and reads from the CentralMessage
    -- channel.
    let
        loop :: ServerState -> IO ()
        loop state =
            atomically (readTChan chan) >>= processCentralMessage chan state >>= loop

        initial :: ServerState
        initial = ServerState
            { clients = IM'.empty
            , internalServerState = Static.Init.init
            , nextClientId = 0
            }
    in
        loop initial


processCentralMessage :: (TChan CentralMessage) -> ServerState -> CentralMessage -> IO ServerState
processCentralMessage centralMessageChan state (NewUser clientMessageChan conn) = do
    -- A new Client has signed on. We need to add the following to the server
    -- state:
    -- (1) Add their ClientMessage channel for broadcasting future updates.
    -- (2) Add their username as a key to the scoring Dict with a fresh value.

    let newClientId = (nextClientId state)

    -- Add the new Client to the server state.
    let nextClients = IM'.insert newClientId (Client clientMessageChan) (clients state)

    -- Construct the next state with the new list of Clients and new
    -- Dict of scores.
    let nextState = state { clients = nextClients, nextClientId = newClientId + 1 }

    -- Run two threads and terminate when either dies.
    -- (1) Process the new message channel responsible for this Client.
    -- (2) Read the WebSocket connection and pass it on to the central
    --     message channel.
    forkIO $ race_ (processClientChan conn clientMessageChan) $ forever (do
        msg <- WS.receiveData conn
        parseIncomingMsg newClientId centralMessageChan msg
        return ())  -- We don't care about the results of `race`

    -- Provide the new state back to the loop.
    return nextState

processCentralMessage _ state (ReceivedMessage clientId incomingMsg) = do
    -- An Client has changed the colour of a pixel. We need to:
    -- (1) Modify the colour Dict in the server state.
    -- (2) Broadcast the new colour to all clients.
    (nextServerState,mMessage) <- update clientId incomingMsg $ internalServerState state
    let nextState = state { internalServerState = nextServerState }

    --Send message to all clients, FIXME: allow choosing of specific clients
    _ <- case mMessage of 
            Just msg -> atomically $ mapM_ (\(Client chan) -> writeTChan chan $ SendMessage msg) $ clients state
            Nothing  -> return ()
    return nextState

--a new message has been received from a client. Process it and inform the central message about it.
parseIncomingMsg :: ClientID -> TChan CentralMessage -> Text -> IO ()
parseIncomingMsg clientId chan msg = do
    case (decodeIncomingMessage (Err "",T.splitOn "\0" msg)) of
        (Ok msg,_) -> do
                        atomically $ writeTChan chan $ ReceivedMessage clientId msg
        (Err er,l) -> Tio.putStrLn $ T.concat ["Error decoding message from client ", T.pack $ show clientId, ". Failed with error: ", er, " and the following still in the deocde buffer:", T.pack $ show l, "."]



processClientChan :: WS.Connection -> TChan ClientMessage -> IO ()
processClientChan conn chan = forever $ do
    -- This reads a ClientMessage channel forever and passes any messages
    -- it reads to the WebSocket Connection.
    (SendMessage outgoingMessage) <- atomically $ readTChan chan
    let txtMsg = encodeOutgoingMessage outgoingMessage

    WS.sendTextData conn txtMsg
    Tio.putStrLn $ T.concat $ ["Sent: ",  txtMsg]|]