module Static.Types exposing(..)
import NewspaperExample.Static.Types


type alias TopLevelData = ()
-- a type identifying all of the nets in the server
type NetModel  =
      NewspaperExample NewspaperExample.Static.Types.NetState
-- a union type of all the nets and their incoming transitions
type NetIncomingMessage  =
      NewspaperExampleInMsg NewspaperExample.Static.Types.IncomingMessage
-- a union type of all the nets and their outgoing transitions
type NetOutgoingTransition  =
      NewspaperExampleOTrans NewspaperExample.Static.Types.OutgoingTransition
