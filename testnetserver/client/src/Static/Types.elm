module Static.Types exposing(..)
import TestNet.Static.Types


-- a type identifying all of the nets in the server
type NetModel  =
      TestNet TestNet.Static.Types.NetState
-- a union type of all the nets and their transitions
type NetTransition  =
      TestNetTrans TestNet.Static.Types.Transition
-- a union type of all the nets and their transitions
type NetOutgoingMessage  =
      TestNetOMsg TestNet.Static.Types.ClientMessage
