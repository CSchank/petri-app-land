module Static.Types where
import NewspaperExample.Static.Types


-- a type identifying all of the nets in the server
data NetModel  =
      NewspaperExample
    deriving(Show,Ord,Eq)
-- a union type of all the nets and their transitions
data NetTransition  =
      NewspaperExampleTrans NewspaperExample.Static.Types.Transition
    deriving(Show,Ord,Eq)
-- a union type of all the nets and their transitions
data NetOutgoingMessage  =
      NewspaperExampleOMsg NewspaperExample.Static.Types.ClientMessage
    deriving(Show,Ord,Eq)
