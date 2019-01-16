module Static.Encode where
import TestNet.Static.Encode as TestNet

import Static.ServerTypes
import Data.Text as T
import Static.Types

encodeOutgoingMessage :: NetOutgoingMessage -> T.Text
encodeOutgoingMessage netTrans =
    case netTrans of
        TestNetOMsg msg -> TestNet.encodeClientMessage msg

