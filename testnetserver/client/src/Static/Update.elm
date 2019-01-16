module Static.Update exposing(..)
import TestNet.Static.Update as TestNet

import Static.Types
import Utils.Utils

update :: TopLevelData -> NetTransition -> ClientState -> (ClientState, Maybe (Cmd NetTransition))
update tld mClientID netTrans state =
    case netTrans of
        TestNetTrans msg ->
            let
                (newNetState, clientMessages, mCmd) = TestNet.update tld mClientID msg (fromJust $ TM.lookup $ serverState state)
                cmd = fmap (\m -> cmdMap TestNetTrans m) mCmd
                cMsgs = map (\(cId,m) -> (cId,TestNetOMsg m)) clientMessages
                newServerState = state { serverState = TM.insert newNetState (serverState state) }
            in (newServerState, cMsgs, cmd)


