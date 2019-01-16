module Static.Decode exposing(..)
import Static.Types
import Utils.Utils
import TestNet.Static.Decode


decodeIncomingMessage :: String -> NetModel -> Result T.Text NetTransition
decodeIncomingMessage txt clientNet =
    case clientNet of
        TestNet -> rMap TestNetTrans $ fst $ TestNet.Static.Decode.decodeTransition (Err "",T.splitOn "\0" txt)
