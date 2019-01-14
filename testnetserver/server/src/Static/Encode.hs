module Static.Encode where
import TestNet.Static.Encode as TestNet

import Static.ServerTypes
import Data.Text as T

encodeOutgoingMessage :: NetOutgoingMessage -> T.Text
encodeOutgoingMessage netTrans state =
    case netTrans of
        TestNetOMsg msg -> TestNet.Static.Encode.encode msg

