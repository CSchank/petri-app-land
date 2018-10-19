{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Map as M
import Data.String

-- paper 1 will stick to messages arriving in order (websockets)
-- paper 2 will allow messages out of order (webrtc)

type ElmDocType = (ElmType,String,String) -- type and name for pattern matching and documentation
                                          -- doc string can be empty, but name string has to be legal Elm name

data ElmType = ElmIntRange Int Int -- upper and lower bounds (used for optimizing messages, etc.)
             | ElmFloatRange Double Double Int -- upper and lower bounds, and number of decimal places of precision
             | ElmString -- a unicode string
             | ElmSizedString Int -- string with maximum size, and restricted character set TBD
             | ElmPair ElmDocType ElmDocType
             | ElmTriple ElmDocType ElmDocType ElmDocType
             | ElmList ElmDocType
             | ElmDict ElmDocType ElmDocType
             | ElmType String --the type referenced must be included in the map
             | ElmWildcardType String -- a type parameter
             | ElmMaybe ElmDocType
             | ElmBool
  deriving (Ord,Eq,Show)

type Constructor = (String,[ElmDocType]) -- (name, arguments)

data ElmCustom = ElmCustom String [Constructor] -- name of the type 
  deriving (Ord,Eq,Show)

type ClientState        = Constructor
type ServerState        = Constructor
data OutgoingClientMessage   = 
      OnlySender        Constructor
    | AllExceptSender   Constructor
    | SenderAnd         Constructor
    | ToAll             Constructor
    | NoClientMessage
  deriving (Ord,Eq,Show)
type ClientTransition   = Constructor
type ServerTransition   = Constructor

type ClientStateDiagram =
    M.Map (String, ClientTransition) (String, Maybe ServerTransition)

type ServerStateDiagram =
    M.Map (String, ServerTransition) (String, OutgoingClientMessage)

type ExtraClientTypes =
    M.Map String ElmCustom

type ExtraServerTypes =
    M.Map String ElmCustom

type ClientStates =
    M.Map String Constructor

type ServerStates =
    M.Map String Constructor

type ClientServerApp =
    ( String                   --starting state of client
    , String                --starting state of server
    , ClientStates          --all possible client states
    , ServerStates          --all possible server states
    , ExtraClientTypes        --extra client types used in states or messages
    , ExtraServerTypes        --extra server types used in states or messages
    , ClientStateDiagram    --the client state diagram
    , ServerStateDiagram    --the client state diagram
    )