module Static.Encode exposing(..)
import NewYouthHack.Static.Encode as NewYouthHack

import Static.Types exposing(NetOutgoingTransition(..))

encodeOutgoingTransition : NetOutgoingTransition -> String
encodeOutgoingTransition netTrans =
    case netTrans of
        NewYouthHackOTrans msg -> NewYouthHack.encodeOutgoingTransition msg

