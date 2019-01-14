module Static.Update where
import TestNet.Static.Update as TestNet

import Static.Types
import Data.TMap as TM

update :: Maybe ClientID -> NetTransition -> ServerState -> (ServerState, [(ClientID,NetOutgoingMessage)], Cmd NetTransition)
update mClientID netTrans state =
    case netTrans of
        TestNet msg -> let
            (newNetState, clientMessages, mCmd) = TestNet.update msg (fromJust $ TM.lookup $ serverState state)
            cmd = fmap (\m -> cmdMap TestNetTrans m) mCmd
            cMsgs = mapMaybe (\(cId,m) -> (cId,fmap TestNetTrans m)) mCmds
            newServerState = state { serverState = TM.insert newNetState (serverState state) }
        in (newServerState, cMsgs, cmd)



disconnect :: ClientID -> NetModel -> ServerState -> ServerState
disconnect clientID netModel state =
    case netModel of
        TestNet {} -> TestNet.disconnect clientID (fromJust $ TM.lookup $ serverState state)

