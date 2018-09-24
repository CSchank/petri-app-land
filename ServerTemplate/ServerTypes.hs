{-# LANGUAGE QuasiQuotes #-}

module ServerTemplate.ServerTypes where

import Text.RawString.QQ
import Data.Text as T

serverTypesHs :: T.Text
serverTypesHs = T.pack $ [r|{-# LANGUAGE OverloadedStrings #-}
module Static.ServerTypes
    ( Colour(..)
    , CentralMessage(..)
    , ClientMessage(..)
    , ServerState(..)
    , Client(..)
    , ClientID
    ) where

import           Control.Concurrent.STM (STM, TChan)
import qualified Data.Map.Strict        as M'
import qualified Data.IntMap.Strict     as IM'
import           Static.Types
import           Network.WebSockets.Connection (Connection)

type ClientID = Int

data CentralMessage
    = NewUser (TChan ClientMessage) Connection    --register a new user on the server
    | ReceivedMessage ClientID IncomingMessage

data ClientMessage 
    = SendMessage OutgoingMessage

newtype Client = Client (TChan ClientMessage)

data ServerState = ServerState
    { clients :: IM'.IntMap Client
    , nextClientId :: ClientID
    , internalServerState :: Model
    }|]