{-# LANGUAGE OverloadedStrings #-}
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
    ) where

import           Control.Concurrent.STM (STM, TQueue)
import qualified Data.Map.Strict        as M'
import qualified Data.IntMap.Strict     as IM'
import           Static.Types
import           Network.WebSockets.Connection (Connection)
import Data.Set as Set

data CentralMessage
    = NewUser (TQueue ClientThreadMessage) Connection    --register a new user on the server
    | UserConnectionLost ClientID
    | ReceivedMessage ClientID ServerMessage
    | GetCurrentState (TQueue ServerState)
    | SetInternalState Model
    | ResetClients

data ClientThreadMessage 
    = SendMessage ClientMessage
    | ResetState

--types of messages that can be sent back from the user update function

data ToSender clientMessage =
      ToSender clientMessage

data ToAllExceptSender clientMessage =
      ToAllExceptSender clientMessage
    | ToAllExceptSenderF (ClientID -> clientMessage)

data ToSenderAnd clientMessage =
      ToSenderAnd (Set.Set ClientID) clientMessage
    | ToSenderAndF (Set.Set ClientID) (ClientID -> clientMessage)

data ToSet clientMessage =
      ToSet (Set.Set ClientID) clientMessage
    | ToSetF (Set.Set ClientID) (ClientID -> clientMessage)

data ToAll clientMessage =
      ToAll clientMessage
    | ToAllF (ClientID -> clientMessage)

data InternalCM clientMessage =
      ICMNoClientMessage
    | ICMToSender clientMessage
    | ICMToAllExceptSender clientMessage
    | ICMToAllExceptSenderF (ClientID -> clientMessage)
    | ICMToSenderAnd (Set.Set ClientID) clientMessage
    | ICMToSenderAndF (Set.Set ClientID) (ClientID -> clientMessage)
    | ICMToSet (Set.Set ClientID) clientMessage
    | ICMToSetF (Set.Set ClientID) (ClientID -> clientMessage)
    | ICMToAll clientMessage
    | ICMToAllF (ClientID -> clientMessage)
    | ICMAllOf [InternalCM clientMessage]

newtype Client = Client (TQueue ClientThreadMessage)

data ServerState = ServerState
    { clients :: IM'.IntMap Client
    , nextClientId :: ClientID
    , internalServerState :: Model
    }

data Cmd msg =
      Cmd (IO msg)