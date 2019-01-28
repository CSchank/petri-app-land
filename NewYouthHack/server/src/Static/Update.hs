module Static.Update where
import NewYouthHack.Static.Update as NewYouthHack

import Static.Types
import qualified Data.TMap as TM
import Static.ServerTypes
import Utils.Utils
import Data.Maybe (fromJust,mapMaybe,isJust)

update :: TopLevelData -> Maybe ClientID -> NetTransition -> ServerState -> (ServerState, [(ClientID,NetOutgoingMessage)], Maybe (Cmd NetTransition))
update tld mClientID netTrans state =
    case netTrans of
        NewYouthHackTrans msg ->
            let
                (newNetState, clientMessages, mCmd) = NewYouthHack.update tld mClientID msg (fromJust $ TM.lookup $ serverState state)
                cmd = fmap (\m -> cmdMap NewYouthHackTrans m) mCmd
                cMsgs = map (\(cId,m) -> (cId,NewYouthHackOMsg m)) clientMessages
                newServerState = state { serverState = TM.insert newNetState (serverState state) }
            in (newServerState, cMsgs, cmd)


clientConnect :: TopLevelData -> ClientID -> ServerState -> ServerState
clientConnect tld clientID state =
    let
        newNetState = NewYouthHack.clientConnect tld clientID (fromJust $ TM.lookup $ serverState state)
    in
        state { serverState = TM.insert newNetState $ serverState state }

disconnect :: TopLevelData -> ClientID -> NetModel -> ServerState -> ServerState
disconnect tld clientID netModel state =
    let
        newNetState =
            case netModel of
                NewYouthHack {} -> NewYouthHack.disconnect tld clientID (fromJust $ TM.lookup $ serverState state)

    in
        state { serverState = TM.insert newNetState $ serverState state }
