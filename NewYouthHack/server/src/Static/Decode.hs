{-# LANGUAGE OverloadedStrings #-}
module Static.Decode where
import Static.Types
import qualified Data.Text as T
import Utils.Utils
import NewYouthHack.Static.Decode


decodeIncomingMessage :: T.Text -> NetModel -> Result T.Text NetTransition
decodeIncomingMessage txt clientNet =
    case clientNet of
        NewYouthHack -> rMap NewYouthHackTrans $ fst $ NewYouthHack.Static.Decode.decodeTransition (Err "",T.splitOn "\0" txt)
