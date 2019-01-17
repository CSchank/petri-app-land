module Static.Update exposing(..)
import TestNet.Static.Update as TestNet

import Static.Types exposing(..)
import Maybe

update : TopLevelData -> NetIncomingMessage -> NetModel -> (NetModel, Maybe (Cmd NetOutgoingTransition))
update tld netInMsg state =
        case (netInMsg,state) of
            (TestNetInMsg msg, TestNet m) ->
                let
                    (newTestNetState, mcmd) = TestNet.update tld msg m
                    newClientState = TestNet newTestNetState
                in (newClientState, Maybe.map (Cmd.map TestNetOTrans) mcmd)



