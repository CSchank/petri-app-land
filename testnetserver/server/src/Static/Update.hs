module Static.Update where
import TestNet.Static.Update as TestNet

import Static.Types
import qualified Data.TMap as TM
import Static.ServerTypes
import Utils.Utils
import Data.Maybe (fromJust,mapMaybe,isJust)

update :: Maybe ClientID -> NetTransition -> ServerState -> (ServerState, [(ClientID,NetOutgoingMessage)], Maybe (Cmd NetTransition))
update mClientID netTrans state =
    case netTrans of
        TestNetTrans msg ->
            let
                (newNetState, clientMessages, mCmd) = TestNet.update mClientID msg (fromJust $ TM.lookup $ serverState state)
                cmd = fmap (\m -> cmdMap TestNetTrans m) mCmd
                cMsgs = map (\(cId,m) -> (cId,TestNetOMsg m)) clientMessages
                newServerState = state { serverState = TM.insert newNetState (serverState state) }
            in (newServerState, cMsgs, cmd)


clientConnect :: ClientID -> ServerState -> ServerState
clientConnect clientID state =
    let
        newNetState = TestNet.clientConnect clientID (fromJust $ TM.lookup $ serverState state)
    in
        state { serverState = TM.insert newNetState $ serverState state }

disconnect :: ClientID -> NetModel -> ServerState -> ServerState
disconnect clientID netModel state =
    let
        newNetState =
            case netModel of
                TestNet {} -> TestNet.disconnect clientID (fromJust $ TM.lookup $ serverState state)

    in
        state { serverState = TM.insert newNetState $ serverState state }
