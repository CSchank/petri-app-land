module Types where

import Data.Map as M

data BasicTypes =
	  PlainType String
	| IntType String
	| DoubleType String
  deriving (Ord,Eq)
instance Show BasicTypes where
	show (PlainType st) = st
	show (IntType st) = st ++ " Int"
	show (DoubleType st) = st ++ " Double"


type ClientState = BasicTypes
type ServerState = BasicTypes
type ClientTransition = BasicTypes
type ServerTransition = BasicTypes

type ClientStateDiagram =
	M.Map (ClientState, ClientTransition) (ClientState, Maybe ServerTransition)

type ServerStateDiagram =
	M.Map (ServerState, ServerTransition) (ClientState, Maybe ClientTransition)

type ClientServerApp =
	(ClientStateDiagram, ServerStateDiagram)