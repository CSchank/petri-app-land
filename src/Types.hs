{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.Text as T
import qualified Data.Map.Strict as M'

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
             | ElmExisting String {-type name-} String {-module to find this type-} --an existing type from another package; this cannot be serialized
             | ElmExistingWParams String {-type name-} [(String, String)] {-type params, module-} String {-module to find this type-} --an existing type from another package; this cannot be serialized
             | ElmWildcardType String -- a type parameter
             | ElmMaybe ElmDocType
             | ElmBool
             | ElmResult ElmDocType ElmDocType
             | ElmEmpty
  deriving (Ord,Eq,Show)

type Constructor = (String,[ElmDocType]) -- (name, arguments)

data ElmCustom = ElmCustom String [Constructor] -- name of the type 
  deriving (Ord,Eq,Show)

type ClientTransition   = Constructor
type ClientCmd          = T.Text
type ServerTransition   = Constructor
type ServerCmd          = T.Text

data HybridPlace =
    HybridPlace 
        T.Text          --name of the place
        [ElmDocType]    --server place state
        [ElmDocType]    --player state
        [ElmDocType]    --client place state
        (Maybe T.Text)  --Maybe the name of a subnet
        (Maybe T.Text, Maybe T.Text) --initial commands
    deriving(Eq,Ord)

data NetTransition
    = NetTransition
        TransitionOrigin
        Constructor                         --message which attempts to fire this transition (must be unique) 
        [(T.Text                            --from place (must appear in map above)
        ,Maybe (T.Text, Constructor))       --to place (must appear in map above) and client message
        ]
        (Maybe ServerCmd)              --whether to issue a command when this transition is fired
    | ClientTransition
        Constructor
        T.Text                              -- the place at which this transition occurs
        (Maybe ClientCmd)
    | CmdTransition         -- a transition from a client which causes a command on the server
        Constructor -- the message coming from the client
        T.Text      -- the place at which this transition occurs
        ServerCmd   -- the type of command this transition causes
    deriving (Eq,Ord)
                      
data TransitionOrigin =
        OriginClientOnly      
    |   OriginEitherPossible  -- clientId is Maybe, with Nothing in case it comes from server
    |   OriginServerOnly
    deriving (Eq,Ord)

-- a net describing a collections of places and transitions
data Net = 
    HybridNet
        T.Text                                  --net name
        T.Text                                  --starting place for client
        [HybridPlace]                           --all the places in this net
        [NetTransition]      --transitions between the places
        [Plugin]                                --a list of plugins to be generated / installed on this net

type ExtraTypes =
    [ElmCustom]

type ClientServerApp =
    ( T.Text                            --starting net for client
    , [Net]                             --all the nets in this client/server app
    , ExtraTypes                        --extra server types used in states or messages
    )

data Plugin = 
      Plugin String {-name-}
    | PluginGen String {-name-} (M'.Map String ElmCustom -> Net -> IO [(FilePath,T.Text)]) {-function to generate the plugin-}

data Language =
    Elm | Haskell
    deriving (Eq)