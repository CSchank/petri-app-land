module Static.Update exposing(..)
import NewspaperExample.Static.Update as NewspaperExample

import Static.Types exposing(..)
import Maybe

update : TopLevelData -> NetIncomingMessage -> NetModel -> (NetModel, Maybe (Cmd NetOutgoingTransition))
update tld netInMsg state =
        case (netInMsg,state) of
            (NewspaperExampleInMsg msg, NewspaperExample m) ->
                let
                    (newNewspaperExampleState, mcmd) = NewspaperExample.update tld msg m
                    newClientState = NewspaperExample newNewspaperExampleState
                in (newClientState, Maybe.map (Cmd.map NewspaperExampleOTrans) mcmd)



