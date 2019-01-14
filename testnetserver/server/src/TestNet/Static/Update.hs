module TestNet.Static.Update where
import TestNet.Static.Types
import TestNet.Static.Wrappers
import Data.TMap as TM

-- player processing functions
processABPlayer :: (APlayer -> ABfromA) -> Player -> (Player, Maybe ClientMessage)
processABPlayer fromA player = case player of
    (A n nLst)  -> unwrapABfromA $ fromA $ wrapAPlayer player


processCAPlayer :: (CPlayer -> CAfromC) -> Player -> (Player, Maybe ClientMessage)
processCAPlayer fromC player = case player of
    (C n nLst)  -> unwrapCAfromC $ fromC $ wrapCPlayer player


processABCPlayer :: (APlayer -> ABCfromA) -> (BPlayer -> ABCfromB) -> Player -> (Player, Maybe ClientMessage)
processABCPlayer fromA fromB player = case player of
    (A n nLst)  -> unwrapABCfromA $ fromA $ wrapAPlayer player
    (B n nLst)  -> unwrapABCfromB $ fromB $ wrapBPlayer player



-- player splitting functions
splitABPlayers :: [(ClientID,Player)] -> ([(ClientID,APlayer)])
splitABPlayers players = foldl (\t@(fromAlst) pl -> case pl of
    (cId,PAPlayer {}) -> ((cId,unwrapFromA pl):fromAlst)

    _ -> t) ([]) players

splitCAPlayers :: [(ClientID,Player)] -> ([(ClientID,CPlayer)])
splitCAPlayers players = foldl (\t@(fromClst) pl -> case pl of
    (cId,PCPlayer {}) -> ((cId,unwrapFromC pl):fromClst)

    _ -> t) ([]) players

splitABCPlayers :: [(ClientID,Player)] -> ([(ClientID,APlayer)],[(ClientID,BPlayer)])
splitABCPlayers players = foldl (\t@(fromAlst,fromBlst) pl -> case pl of
    (cId,PAPlayer {}) -> ((cId,unwrapFromA pl):fromAlst,fromBlst)
    (cId,PBPlayer {}) -> (fromAlst,(cId,unwrapFromB pl):fromBlst)

    _ -> t) ([],[]) players


update :: Transition -> NetState Player -> (NetState Player,[(ClientID,ClientMessage)],Maybe (Cmd Transition))
update trans state =
    let
        places = placeStates state
        players = playerStates state
        (newPlaces, newPlayers, clientMessages, cmd) = 
            case trans of
                (TAB n) ->
                    let
                        (aPlayerLst) = splitABPlayers players
                        (a,b,fromA) = updateAB (fromJust $ TM.lookup places) (fromJust $ TM.lookup places) aPlayerLst
                        newPlaces = TM.insert a places
                        (newPlayers, clientMessages) = unzip $ map (processABPlayer fromA) players
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)

                (TCA n) ->
                    let
                        (cPlayerLst) = splitCAPlayers players
                        (c,a,fromC) = updateCA (fromJust $ TM.lookup places) (fromJust $ TM.lookup places) cPlayerLst
                        newPlaces = TM.insert c places
                        (newPlayers, clientMessages) = unzip $ map (processCAPlayer fromC) players
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)

                (TABC n) ->
                    let
                        (aPlayerLst,bPlayerLst) = splitABCPlayers players
                        (a,b,c,fromA,fromB) = updateABC (fromJust $ TM.lookup places) (fromJust $ TM.lookup places) (fromJust $ TM.lookup places) aPlayerLst bPlayerLst
                        newPlaces = TM.insert a $ TM.insert b places
                        (newPlayers, clientMessages) = unzip $ map (processABCPlayer fromA fromB) players
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)


    in
        (state
           {
                placeStates = newPlaces
           ,    playerStates = newPlayers
           }
        , clientMessages
        , cmd)
