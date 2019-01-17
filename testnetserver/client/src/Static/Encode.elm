module Static.Encode exposing(..)
import TestNet.Static.Encode as TestNet

import Static.Types exposing(NetOutgoingTransition(..))

encodeOutgoingTransition : NetOutgoingTransition -> String
encodeOutgoingTransition netTrans =
    case netTrans of
        TestNetOTrans msg -> TestNet.encodeOutgoingTransition msg

