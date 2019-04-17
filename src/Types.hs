{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.Text as T
import qualified Data.Map.Strict as M'

-- paper 1 will stick to messages arriving in order (websockets)
-- paper 2 will allow messages out of order (webrtc)

type DocTypeT = (TypeT,T.Text,T.Text) -- type and name for pattern matching and documentation
                                          -- doc string can be empty, but name string has to be legal Elm name

data TypeT = 
      IntRangeT Int Int -- upper and lower bounds (used for optimizing messages, etc.)
    | FloatRangeT Double Double Int -- upper and lower bounds, and number of decimal places of precision
    | StringT -- a unicode string
    | SizedStringT Int -- string with maximum size, and restricted character set TBD
    | PairT DocTypeT DocTypeT
    | TripleT DocTypeT DocTypeT DocTypeT
    | ListT DocTypeT
    | DictT DocTypeT DocTypeT
    | TypeT T.Text --the type referenced must be included in the map
    | ExistingT T.Text {-type name-} T.Text {-module to find this type-} --an existing type from another package; this cannot be serialized
    | ExistingWParamsT T.Text {-type name-} [(T.Text, T.Text)] {-type params, module-} T.Text {-module to find this type-} --an existing type from another package; this cannot be serialized
    | WildcardTypeT T.Text -- a type parameter
    | MaybeT DocTypeT
    | BoolT
    | ResultT DocTypeT DocTypeT
    | EmptyT
  deriving (Ord,Eq,Show)

type Constructor = (T.Text,[DocTypeT]) -- (name, arguments)

data CustomT = CustomT T.Text [Constructor] -- name of the type 
  deriving (Ord,Eq,Show)

type ClientCmd          = T.Text
type ServerCmd          = T.Text

data Place =
    Place 
        T.Text          --name of the place
        [DocTypeT]    --server place state
        [DocTypeT]    --player state
        [DocTypeT]    --client place state
        ([ServerCmd]) --initial server commands
    deriving(Eq,Ord)

type Connection =
    (T.Text                             -- from place (must appear in map above)
        ,Maybe (T.Text                  -- to place (must appear in map above) and client message
                , Constructor           -- message sent to client
                , [ClientCmd]))     -- a client command to be issued once the message is received on the client side
        

data Transition
    = Transition
        TransitionOrigin                -- where the transition can come from
        Constructor                     -- message which initiates this transition (must be unique) 
        [Connection]                    -- a list of connections involved in this transition
        [ServerCmd]               -- whether to issue a server command when this transition is fired
    | ClientTransition
        Constructor                     -- this transition's name and data
        T.Text                          -- the place at which this transition occurs
        [ClientCmd]               -- whether to issue a client command when this transition is fired
    | CmdTransition                     -- a transition from a client which causes a command on the server
        Constructor                     -- the message coming from the client
        T.Text                          -- the place at which this transition occurs
        ServerCmd                       -- the type of command this transition causes
    deriving (Eq,Ord)
                      

data TransitionOrigin =
        OriginClientOnly      
    |   OriginEitherPossible  -- clientId is Maybe, with Nothing in case it comes from server
    |   OriginServerOnly
    deriving (Eq,Ord)

-- a net describing a collections of places and transitions
data Net = 
    Net
        T.Text                                  --net name
        T.Text                                  --starting place for client
        [Place]                           --all the places in this net
        [Transition]      --transitions between the places
        [Plugin]                                --a list of plugins to be generated / installed on this net

type ExtraTypes =
    [CustomT]

type ClientServerApp =
    ( T.Text                            --starting net for client
    , [Net]                             --all the nets in this client/server app
    , ExtraTypes                        --extra server types used in states or messages
    )

--type for describing an installed plugin, or how to generated a plugin
data Plugin = 
      Plugin T.Text {-name-}
    | PluginGen T.Text {-name-} (M'.Map T.Text CustomT -> Net -> IO [(FilePath,T.Text)]) {-function to generate the plugin-}

data Language =
    Elm | Haskell
    deriving (Eq)