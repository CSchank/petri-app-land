module TestNet.Static.Update exposing(..)
import TestNet.Static.Types exposing(..)
import TestNet.Static.Wrappers exposing(..)
import TestNet.Static.FromSuperPlace exposing (FromSuperPlace)
import TestNet.Update exposing(..)

update : FromSuperPlace -> IncomingMessage -> NetState -> (NetState,Maybe (Cmd OutgoingTransition))
update fsp trans state =
    case (trans,state) of
        ((MStartGameAB _) , SA st) -> (SB <| updateAStartGameABB fsp (wrapStartGameAB trans) st, Nothing)

        ((MStartGameCA _) , SC st) -> (SA <| updateCStartGameCAA fsp (wrapStartGameCA trans) st, Nothing)

        ((MStartGameAB2 _) , SA st) -> (SB <| updateAStartGameAB2B fsp (wrapStartGameAB2 trans) st, Nothing)
        ((MStartGameAC _) , SA st) -> (SC <| updateAStartGameACC fsp (wrapStartGameAC trans) st, Nothing)
        ((MStartGameBC _) , SB st) -> (SC <| updateBStartGameBCC fsp (wrapStartGameBC trans) st, Nothing)


        _ -> (state, Nothing)
