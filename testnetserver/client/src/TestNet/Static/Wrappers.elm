module TestNet.Static.Wrappers exposing(..)
import TestNet.Static.Types exposing(..)

wrapStartGameAB : IncomingMessage -> StartGameAB
wrapStartGameAB x__ =
    case x__ of
        (MStartGameAB n)  -> (StartGameAB n) 
        _ -> StartGameAB 0


wrapStartGameCA : IncomingMessage -> StartGameCA
wrapStartGameCA x__ =
    case x__ of
        (MStartGameCA n)  -> (StartGameCA n) 
        _ -> StartGameCA 0


wrapStartGameAB2 : IncomingMessage -> StartGameAB2
wrapStartGameAB2 x__ =
    case x__ of
        (MStartGameAB2 n)  -> (StartGameAB2 n) 
        _ -> StartGameAB2 0


wrapStartGameAC : IncomingMessage -> StartGameAC
wrapStartGameAC x__ =
    case x__ of
        (MStartGameAC n)  -> (StartGameAC n) 
        _ -> StartGameAC 0


wrapStartGameBC : IncomingMessage -> StartGameBC
wrapStartGameBC x__ =
    case x__ of
        (MStartGameBC n)  -> (StartGameBC n) 
        _ -> StartGameBC 0



unwrapCA : CA -> OutgoingTransition
unwrapCA (CA n)  = (TCA n) 


unwrapABC : ABC -> OutgoingTransition
unwrapABC (ABC n)  = (TABC n) 



