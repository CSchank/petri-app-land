{-# LANGUAGE OverloadedStrings #-}
module Static.NetLogic (..) where

import           Control.Concurrent.STM (STM, TQueue, atomically, newTQueue,
                                         readTQueue, writeTQueue)
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


newNetMessageChan :: STM (TQueue (NetMsg netMsg))
newNetMessageChan =
    newTQueue

newClientMessageChan :: STM (TQueue ClientThreadMessage)
newClientMessageChan =
    newTQueue

processCentralChan :: TQueue (NetMsg netMsg) -> playerState -> IO ()
processCentralChan chan initPlayerState =
    -- This loop holds the server state and reads from the CentralMessage
    -- channel.
    let
        loop :: NetState playerState -> IO ()
        loop state =
            atomically (readTQueue chan) >>= processCentralMessage chan state >>= loop

        initial :: PluginState -> NetState playerState
        initial ps = ServerState
            { clientStates = IM'.empty                      -- the clients in the net (parameterized by that net's player state)
            , placeStates = TMap.empty                      -- the place states in this net
            , pluginStates = TMap.empty                     -- state of the plugins in this net
            , initialPlayerState = initPlayerState          -- initial player state
            }
    in do
        --ps <- initStateCmds
        loop (initial TMap.empty)


processNetMessage :: TQueue (NetMsg netMsg) -> NetState playerState -> NetMsg netMsg -> IO (NetState playerState)
processNetMessage netMsgChan state (ConnectClient clientID) = do
    let nextClients = IM'.insert clientID (initialPlayerState state) (clientStates state)

    -- Provide the new state back to the loop.
    return $ state { clientStates = nextClients }

processNetMessage netMsgChan state (DisconnectClient clientID) = do
    let nextClients = IM'.remove clientID (clientStates state)

    -- Provide the new state back to the loop.
    return $ state { clientStates = nextClients }

--a new message has been received from a client. Process it and inform the central message about it.
parseIncomingMsg :: ClientID -> TQueue CentralMessage -> Text -> IO ()
parseIncomingMsg clientId chan msg = do
    case (decodeServerMessage (Err "",T.splitOn "\0" msg)) of
        (Ok msg,_) -> do
                        atomically $ writeTQueue chan $ ReceivedMessage clientId msg
        (Err er,l) -> Tio.putStrLn $ T.concat ["Error decoding message from client ", T.pack $ show clientId, ". Failed with error: ", er, " and the following still in the deocde buffer:", T.pack $ show l, "."]



processClienTQueue :: WS.Connection -> TQueue ClientThreadMessage -> IO ()
processClienTQueue conn chan = forever $ do
    -- This reads a ClientThreadMessage channel forever and passes any messages
    -- it reads to the WebSocket Connection.
    clientMsg{-(SendMessage outgoingMessage)-} <- atomically $ readTQueue chan
    case clientMsg of 
        SendMessage outgoingMessage -> do
            let txtMsg = encodeClientMessage outgoingMessage

            WS.sendTextData conn txtMsg
            Tio.putStrLn $ T.concat $ ["Sent: ",  txtMsg]
        ResetState ->
            WS.sendTextData conn ("resetfadsfjewi" :: T.Text)