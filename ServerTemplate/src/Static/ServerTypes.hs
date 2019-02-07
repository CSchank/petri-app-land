{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}
module Static.ServerTypes where

import           Control.Concurrent.STM (STM, TQueue, TVar)
import qualified Data.Map.Strict        as M'
import qualified Data.IntMap.Strict     as IM'
import           Static.Types
import           Network.WebSockets.Connection (Connection)
import Data.Set as Set
import Data.TMap (TMap)
import Data.Typeable (Typeable)
import qualified Data.Text as T

type ClientID = Int

data CentralMessage
    = NewUser (TQueue OutgoingClientThreadMessage) Connection    --register a new user on the server
    | UserConnectionLost ClientID
    | ReceivedMessage 
        (Maybe ClientID)    -- Just: message from a client, Nothing: message from a command
        NetTransition       -- message that was sent

data OutgoingClientThreadMessage
    = SendMessage T.Text    -- send message to client
   -- | ResetState

data Client = 
    Client (TVar NetModel) (TQueue OutgoingClientThreadMessage) NetID {-current net for this user, for decoding-}

type NetID = Int

data ServerState = ServerState
    { clients :: IM'.IntMap Client
    , nextClientId :: ClientID
    , serverState :: TMap                       -- TMap of all the NetStates
    , startTime :: Int                          -- Unix time the server was started
    }

data NetState playerState = NetState
    { playerStates :: IM'.IntMap playerState    -- the clients in the net (parameterized by that net's player state)
    , placeStates :: TMap                       -- the place states in this net
    , pluginStates :: TMap                      -- state of the plugins in this net
    }

-- data that the top-level update functions receive
data TopLevelData = TopLevelData
    {
        serverStartTime :: Int
    ,   currentTime :: Int
    }

type PluginState = TMap

{- message type for each net's STM loop
data NetMsg netMsg = 
      ConnectClient ClientID
    | DisconnectClient ClientID
    | ReceiveClientMessage ClientID netMsg
    | ReceiveServerMessage netMsg
    -- | MoveClient ClientID NetID -- move a client to another net, gotta think this through a bit better
-}
data Cmd msg =
      Cmd (IO msg)
    | forall state. Plugin state => StateCmd (state -> IO msg)

class (Typeable state) => Plugin state where
    initPlugin :: IO state --initialize the plugin and return its singleton state