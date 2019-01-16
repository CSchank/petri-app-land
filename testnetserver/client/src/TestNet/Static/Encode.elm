module TestNet.Static.Encode exposing(..)
import TestNet.Static.Types

import Utils.Utils
import Static.Types
encodeOutgoingTransition : OutgoingTransition -> String
encodeOutgoingTransition outgoingtransition = 
    case outgoingtransition of
        TCA n -> 
            let
                nTxt = encodeInt 0 1000 n
            in
                tConcat ["TCA\u{0000}", nTxt]
        TABC n -> 
            let
                nTxt = encodeInt 0 1000 n
            in
                tConcat ["TABC\u{0000}", nTxt]


