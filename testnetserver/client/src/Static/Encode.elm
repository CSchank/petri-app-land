module Static.Encode exposing(..)
import TestNet.Static.Encode as TestNet

import Static.Types

encodeOutgoingMessage :: NetOutgoingMessage -> String
encodeOutgoingMessage netTrans =
    case netTrans of
        TestNetOMsg msg -> TestNet.encodeClientMessage msg

