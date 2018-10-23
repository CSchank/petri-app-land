{-# LANGUAGE QuasiQuotes #-}

module ServerTemplate.ServerTypes where

import Text.RawString.QQ
import Data.Text as T

serverTypesHs :: T.Text
serverTypesHs = T.pack $ [r|{-# LANGUAGE OverloadedStrings #-}
module Static.ServerTypes
    ( Colour(..)
    , CentralMessage(..)
    , ClientThreadMessage(..)
    , ServerState(..)
    , Client(..)
    , ClientID
    , OnlySender(..)
    , AllExceptSender(..)
    , SenderAnd(..)
    , ToAll(..)
    , InternalCM(..)
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

data ClientThreadMessage 
    = SendMessage ClientMessage

--types of messages that can be sent back from the user update function

data OnlySender clientMessage =
      OnlySender clientMessage

data AllExceptSender clientMessage =
      AllExceptSender clientMessage
    | AllExceptSenderF (ClientID -> clientMessage)

data SenderAnd clientMessage =
      SenderAnd (Set.Set ClientID) clientMessage
    | SenderAndF (Set.Set ClientID) (ClientID -> clientMessage)

data ToAll clientMessage =
      ToAll clientMessage
    | ToAllF (ClientID -> clientMessage)

data InternalCM clientMessage =
      ICMOnlySender clientMessage
    | ICMAllExceptSender clientMessage
    | ICMAllExceptSenderF (ClientID -> clientMessage)
    | ICMSenderAnd (Set.Set ClientID) clientMessage
    | ICMSenderAndF (Set.Set ClientID) (ClientID -> clientMessage)
    | ICMToAll clientMessage
    | ICMToAllF (ClientID -> clientMessage)

newtype Client = Client (TQueue ClientThreadMessage)

data ServerState = ServerState
    { clients :: IM'.IntMap Client
    , nextClientId :: ClientID
    , internalServerState :: Model
    }|]