module Static.Update exposing(..)
import NewYouthHack.Static.Update as NewYouthHack

import Static.Types exposing(..)
import Maybe

update : TopLevelData -> NetIncomingMessage -> NetModel -> (NetModel, Maybe (Cmd NetOutgoingTransition))
update tld netInMsg state =
        case (netInMsg,state) of
            (NewYouthHackInMsg msg, NewYouthHack m) ->
                let
                    (newNewYouthHackState, mcmd) = NewYouthHack.update tld msg m
                    newClientState = NewYouthHack newNewYouthHackState
                in (newClientState, Maybe.map (Cmd.map NewYouthHackOTrans) mcmd)



