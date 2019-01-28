module Static.Types exposing(..)
import NewYouthHack.Static.Types


type alias TopLevelData = ()
-- a type identifying all of the nets in the server
type NetModel  =
      NewYouthHack NewYouthHack.Static.Types.NetState
-- a union type of all the nets and their incoming transitions
type NetIncomingMessage  =
      NewYouthHackInMsg NewYouthHack.Static.Types.IncomingMessage
-- a union type of all the nets and their outgoing transitions
type NetOutgoingTransition  =
      NewYouthHackOTrans NewYouthHack.Static.Types.OutgoingTransition
