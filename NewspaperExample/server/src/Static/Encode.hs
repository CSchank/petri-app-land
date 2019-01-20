module Static.Encode where
import NewspaperExample.Static.Encode as NewspaperExample

import Static.ServerTypes
import Data.Text as T
import Static.Types

encodeOutgoingMessage :: NetOutgoingMessage -> T.Text
encodeOutgoingMessage netTrans =
    case netTrans of
        NewspaperExampleOMsg msg -> NewspaperExample.encodeClientMessage msg

