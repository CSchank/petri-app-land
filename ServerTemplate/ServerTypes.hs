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
    ) where

import           Control.Concurrent.STM (STM, TChan)
import qualified Data.Map.Strict        as M'
import qualified Data.IntMap.Strict     as IM'
import           Static.Types
import           Network.WebSockets.Connection (Connection)

type ClientID = Int

data CentralMessage
    = NewUser (TChan ClientThreadMessage) Connection    --register a new user on the server
    | ReceivedMessage ClientID ServerMessage

data ClientThreadMessage 
    = SendMessage ClientMessage

newtype Client = Client (TChan ClientThreadMessage)

data ServerState = ServerState
    { clients :: IM'.IntMap Client
    , nextClientId :: ClientID
    , internalServerState :: Model
    }|]