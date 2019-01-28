module Static.Encode where
import NewYouthHack.Static.Encode as NewYouthHack

import Static.ServerTypes
import Data.Text as T
import Static.Types

encodeOutgoingMessage :: NetOutgoingMessage -> T.Text
encodeOutgoingMessage netTrans =
    case netTrans of
        NewYouthHackOMsg msg -> NewYouthHack.encodeClientMessage msg

