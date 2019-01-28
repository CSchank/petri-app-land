module Static.Types where
import NewYouthHack.Static.Types


-- a type identifying all of the nets in the server
data NetModel  =
      NewYouthHack
    deriving(Show,Ord,Eq)
-- a union type of all the nets and their transitions
data NetTransition  =
      NewYouthHackTrans NewYouthHack.Static.Types.Transition
    deriving(Show,Ord,Eq)
-- a union type of all the nets and their transitions
data NetOutgoingMessage  =
      NewYouthHackOMsg NewYouthHack.Static.Types.ClientMessage
    deriving(Show,Ord,Eq)
