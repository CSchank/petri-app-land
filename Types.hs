{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Map as M
import Data.String

-- paper 1 will stick to messages arriving in order (websockets)
-- paper 2 will allow messages out of order (webrtc)

type ElmDocType = (ElmType,String,String) -- type and name for pattern matching and documentation
                                          -- doc string can be empty, but name string has to be legal Elm name

data ElmType = ElmInt
		     | ElmIntRange Int Int -- upper and lower bounds (used for optimizing messages, etc.) 
			 | ElmFloat
			 | ElmFloatRange Float Float Int -- upper and lower bounds, and 
			 | ElmString -- a unicode string
			 | ElmSizedString Int -- string with maximum size, and restricted character set TBD
			 | ElmPair ElmDocType ElmDocType
			 | ElmTriple ElmDocType ElmDocType ElmDocType
			 | ElmList ElmDocType
			 | ElmType String --the type referenced must be included in the map
  deriving (Ord,Eq,Show)

data ElmCustom = ElmCustom String          	-- name of the type 
						[	(String      	-- constructor name
						,[ElmDocType])] 	-- constructor arguments
  deriving (Ord,Eq,Show)

data BasicTypes =
	  PlainType String
	| IntType String
	| DoubleType String
  deriving (Ord,Eq)

instance Show BasicTypes where
	show (PlainType st) = st
	show (IntType st) = st ++ " Int"
	show (DoubleType st) = st ++ " Double"
instance IsString BasicTypes where
	fromString s = PlainType s


type ClientState = ElmCustom
type ServerState = ElmCustom
type ClientTransition = ElmCustom
type ServerTransition = ElmCustom

type ClientStateDiagram =
	M.Map (ClientState, ClientTransition) (ClientState, Maybe ServerTransition)

type ServerStateDiagram =
	M.Map (ServerState, ServerTransition) (ClientState, Maybe ClientTransition)

type ClientServerApp =
	(ClientState, ServerState, ClientStateDiagram, ServerStateDiagram)