{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}
module Static.ServerTypes
    ( CentralMessage(..)
    , ClientThreadMessage(..)
    , ServerState(..)
    , Client(..)
    , ClientID
    , ToSender(..)
    , ToAllExceptSender(..)
    , ToSenderAnd(..)
    , ToSet(..)
    , ToAll(..)
    , InternalCM(..)
    , Cmd(..)
    , Plugin(..)
    , PluginState
    ) where

import           Control.Concurrent.STM (STM, TQueue, TVar)
import qualified Data.Map.Strict        as M'
import qualified Data.IntMap.Strict     as IM'
import           Static.Types
import           Network.WebSockets.Connection (Connection)
import Data.Set as Set
import Data.TMap (TMap)
import Data.Typeable (Typeable)

data CentralMessage
    = NewUser (TQueue OutgoingClientThreadMessage) Connection    --register a new user on the server
    | UserConnectionLost ClientID
    | ReceivedMessage ClientID ServerMessage

data OutgoingClientThreadMessage
    = SendMessage T.Text    -- send message to client
   -- | ResetState

data Client = 
    Client (TVar NetModel) (TQueue OutgoingClientThreadMessage) NetID {-current net for this user, for decoding-}

type NetID = Int

data ServerState = ServerState
    { clients :: IM'.IntMap Client
    , nextClientId :: ClientID
    , serverState :: TMap                       -- TMap of all the nets
    , startTime :: Int                          -- Unix time the server was started
    }

data NetState playerState = NetState
    { playerStates :: IM'.IntMap playerState    -- the clients in the net (parameterized by that net's player state)
    , placeStates :: TMap                       -- the place states in this net
    , pluginStates :: TMap                      -- state of the plugins in this net
    }

-- data that the top-level update functions receive
type TopLevelData = 
    (Int {-start time-}, Int {-time-})

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