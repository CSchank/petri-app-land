module TestNet.Static.Wrappers exposing(..)
import TestNet.Static.Types

unwrapStartGameAB :: StartGameAB -> ClientMessage
unwrapStartGameAB (StartGameAB n)  = (MStartGameAB n) 


unwrapStartGameCA :: StartGameCA -> ClientMessage
unwrapStartGameCA (StartGameCA n)  = (MStartGameCA n) 


unwrapStartGameAB2 :: StartGameAB2 -> ClientMessage
unwrapStartGameAB2 (StartGameAB2 n)  = (MStartGameAB2 n) 


unwrapStartGameAC :: StartGameAC -> ClientMessage
unwrapStartGameAC (StartGameAC n)  = (MStartGameAC n) 


unwrapStartGameBC :: StartGameBC -> ClientMessage
unwrapStartGameBC (StartGameBC n)  = (MStartGameBC n) 



wrapStartGameAB :: ClientMessage -> StartGameAB
wrapStartGameAB x__ =
    case x__ of
        (MStartGameAB n)  -> (StartGameAB n) 
        _ -> error "Tried to wrap a value at the wrong time!"


wrapStartGameCA :: ClientMessage -> StartGameCA
wrapStartGameCA x__ =
    case x__ of
        (MStartGameCA n)  -> (StartGameCA n) 
        _ -> error "Tried to wrap a value at the wrong time!"


wrapStartGameAB2 :: ClientMessage -> StartGameAB2
wrapStartGameAB2 x__ =
    case x__ of
        (MStartGameAB2 n)  -> (StartGameAB2 n) 
        _ -> error "Tried to wrap a value at the wrong time!"


wrapStartGameAC :: ClientMessage -> StartGameAC
wrapStartGameAC x__ =
    case x__ of
        (MStartGameAC n)  -> (StartGameAC n) 
        _ -> error "Tried to wrap a value at the wrong time!"


wrapStartGameBC :: ClientMessage -> StartGameBC
wrapStartGameBC x__ =
    case x__ of
        (MStartGameBC n)  -> (StartGameBC n) 
        _ -> error "Tried to wrap a value at the wrong time!"



unwrapABfromA :: ABfromA -> (Player, Maybe ClientMessage)
unwrapABfromA trans =
    case trans of
        (AB_AtoB player msg)  -> (unwrapBPlayer player, Just $ unwrapStartGameAB msg)




unwrapCAfromC :: CAfromC -> (Player, Maybe ClientMessage)
unwrapCAfromC trans =
    case trans of
        (CA_CtoA player msg)  -> (unwrapAPlayer player, Just $ unwrapStartGameCA msg)




unwrapABCfromA :: ABCfromA -> (Player, Maybe ClientMessage)
unwrapABCfromA trans =
    case trans of
        (ABC_AtoC player msg)  -> (unwrapCPlayer player, Just $ unwrapStartGameAC msg)
        (ABC_AtoB player msg)  -> (unwrapBPlayer player, Just $ unwrapStartGameAB2 msg)



unwrapABCfromB :: ABCfromB -> (Player, Maybe ClientMessage)
unwrapABCfromB trans =
    case trans of
        (ABC_BtoC player msg)  -> (unwrapCPlayer player, Just $ unwrapStartGameBC msg)





unwrapAB :: AB -> Transition
unwrapAB (AB n)  = (TAB n) 


unwrapCA :: CA -> Transition
unwrapCA (CA n)  = (TCA n) 


unwrapABC :: ABC -> Transition
unwrapABC (ABC n)  = (TABC n) 



wrapAB :: Transition -> AB
wrapAB x__ =
    case x__ of
        (TAB n)  -> (AB n) 
        _ -> error "Tried to wrap a value at the wrong time!"


wrapCA :: Transition -> CA
wrapCA x__ =
    case x__ of
        (TCA n)  -> (CA n) 
        _ -> error "Tried to wrap a value at the wrong time!"


wrapABC :: Transition -> ABC
wrapABC x__ =
    case x__ of
        (TABC n)  -> (ABC n) 
        _ -> error "Tried to wrap a value at the wrong time!"



