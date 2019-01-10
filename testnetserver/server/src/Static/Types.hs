module Static.Types where
import TestNet.Static.Types


-- a type identifying all of the nets in the server
data NetModel  =
      TestNet
    deriving(Show,Ord,Eq)
-- a union type of all the nets and their transitions
data NetTransitions  =
      TestNetTrans TestNet.Static.Types.Transition
    deriving(Show,Ord,Eq)
