module Static.Encode exposing(..)
import NewspaperExample.Static.Encode as NewspaperExample

import Static.Types exposing(NetOutgoingTransition(..))

encodeOutgoingTransition : NetOutgoingTransition -> String
encodeOutgoingTransition netTrans =
    case netTrans of
        NewspaperExampleOTrans msg -> NewspaperExample.encodeOutgoingTransition msg

