module TestNet.Static.Update where
import TestNet.Static.Types

processABPlayer :: Player -> (Player, Maybe ClientMessage)
processABPlayer = case player of
    (A n nLst)  -> unwrapABfromA


processCAPlayer :: Player -> (Player, Maybe ClientMessage)
processCAPlayer = case player of
    (C n nLst)  -> unwrapCAfromC


processABCPlayer :: Player -> (Player, Maybe ClientMessage)
processABCPlayer = case player of
    (A n nLst)  -> unwrapABCfromA
    (B n nLst)  -> unwrapABCfromB



update :: Transition -> NetState Player -> [(ClientID,Player)] -> (NetState Player,[ClientMessage],Maybe (Cmd Transition),[(ClientID,Player)])
update trans state players =
    let
        processPlayer :: Player -> Player
        processPlayer player = case player of
