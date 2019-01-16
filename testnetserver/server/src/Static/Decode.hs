{-# LANGUAGE OverloadedStrings #-}
module Static.Decode where
import Static.Types
import qualified Data.Text as T
import Utils.Utils
import TestNet.Static.Decode


decodeIncomingMessage :: T.Text -> NetModel -> Result T.Text NetTransition
decodeIncomingMessage txt clientNet =
    case clientNet of
        TestNet -> rMap TestNetTrans $ fst $ TestNet.Static.Decode.decodeTransition (Err "",T.splitOn "\0" txt)
