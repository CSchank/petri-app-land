module Static.Types exposing(..)
import TestNet.Static.Types


-- a type identifying all of the nets in the server
type NetModel  =
      TestNet TestNet.Static.Types.NetState
-- a union type of all the nets and their incoming transitions
type NetIncomingMessage  =
      TestNetInMsg TestNet.Static.Types.IncomingMessage
-- a union type of all the nets and their outgoing transitions
type NetOutgoingTransition  =
      TestNetOTrans TestNet.Static.Types.OutgoingTransition
