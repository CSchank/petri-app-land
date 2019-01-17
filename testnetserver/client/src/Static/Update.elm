module Static.Update exposing(..)
import TestNet.Static.Update as TestNet

import Static.Types
import Utils.Utils

update : TopLevelData -> NetTransition -> ClientState -> (ClientState, Maybe (Cmd NetTransition))
update tld mClientID netTrans state =
