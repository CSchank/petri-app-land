module Static.Decode where
import Static.Types
import qualified Data.Text as T
import Utils.Utils
import TestNet.Static.Encode


decodeIncomingMessage :: T.Text -> NetModel -> Result T.Text NetTransitions
decodeIncomingMessage txt clientNet =
    case clientNet of
        TestNet -> rMap TestNetTrans $ fst $ TestNet.Static.Decode.decodeIncomingMessage (Err "",T.splitOn "\0" txt)
