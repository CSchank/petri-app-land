module TestNet.Static.Update exposing(..)
import TestNet.Static.Types
import TestNet.Static.Wrappers
import TestNet.Static.FromSuperPlace exposing (FromSuperPlace(..))
import TestNet.Update as Update

update :: TopLevelData -> Transition -> NetState -> (NetState,Maybe (Cmd Transition))
update tld mClientID trans state =
    let
        places = placeStates state
        players = playerStates state
        (newPlaces, newPlayers, clientMessages, cmd) = 
            case trans of
                (TAB n) ->
                    let
                        (aPlayerLst) = splitABPlayers (IM'.toList players)
                        (a,b,fromA) = updateAB tld (wrapAB trans) (fromJust $ TM.lookup places) (fromJust $ TM.lookup places) (map snd aPlayerLst)
                        newPlaces = TM.insert a $ TM.insert b places
                        (newPlayers, clientMessages) = unzip $ map (processABPlayer fromA) (IM'.toList players)
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)

                (TCA n) ->
                    let
                        (cPlayerLst) = splitCAPlayers (IM'.toList players)
                        (c,a,fromC) = updateCA tld (fromJust mClientID) (wrapCA trans) (fromJust $ TM.lookup places) (fromJust $ TM.lookup places) (map snd cPlayerLst)
                        newPlaces = TM.insert c $ TM.insert a places
                        (newPlayers, clientMessages) = unzip $ map (processCAPlayer fromC) (IM'.toList players)
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)

                (TABC n) ->
                    let
                        (aPlayerLst,bPlayerLst) = splitABCPlayers (IM'.toList players)
                        (a,b,c,fromA,fromB) = updateABC tld mClientID (wrapABC trans) (fromJust $ TM.lookup places) (fromJust $ TM.lookup places) (fromJust $ TM.lookup places) (map snd aPlayerLst) (map snd bPlayerLst)
                        newPlaces = TM.insert a $ TM.insert b $ TM.insert c places
                        (newPlayers, clientMessages) = unzip $ map (processABCPlayer fromA fromB) (IM'.toList players)
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)


    in
        (state
           {
                placeStates = newPlaces
           ,    playerStates = IM'.fromList newPlayers
           }
        , mapMaybe (\(a,b) -> if isJust b then Just (a,fromJust b) else Nothing) clientMessages
        , cmd)
