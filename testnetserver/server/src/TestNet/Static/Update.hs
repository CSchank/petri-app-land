module TestNet.Static.Update where
import TestNet.Static.Types
import TestNet.Static.Wrappers
import TestNet.Static.FromSuperPlace (FromSuperPlace(..))
import TestNet.Update as Update
import qualified Data.TMap as TM
import Static.ServerTypes
import qualified Data.IntMap.Strict as IM'
import Data.Maybe (fromJust, isJust, mapMaybe)

-- player processing functions
processABPlayer :: (APlayer -> ABfromA) -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))
processABPlayer fromA (cId,player) = case player of
    (PAPlayer playerN)  -> let (np, mCm) = (unwrapABfromA $ fromA $ wrapAPlayer player) in ((cId, np), (cId, mCm))


processCAPlayer :: (CPlayer -> CAfromC) -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))
processCAPlayer fromC (cId,player) = case player of
    (PCPlayer playerN)  -> let (np, mCm) = (unwrapCAfromC $ fromC $ wrapCPlayer player) in ((cId, np), (cId, mCm))


processABCPlayer :: (APlayer -> ABCfromA) -> (BPlayer -> ABCfromB) -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))
processABCPlayer fromA fromB (cId,player) = case player of
    (PAPlayer playerN)  -> let (np, mCm) = (unwrapABCfromA $ fromA $ wrapAPlayer player) in ((cId, np), (cId, mCm))
    (PBPlayer playerN)  -> let (np, mCm) = (unwrapABCfromB $ fromB $ wrapBPlayer player) in ((cId, np), (cId, mCm))



-- player splitting functions
splitABPlayers :: [(ClientID,Player)] -> ([(ClientID,APlayer)])
splitABPlayers players = foldl (\t@(fromAlst) pl -> case pl of
    (cId,p@(PAPlayer {})) -> ((cId,wrapAPlayer p):fromAlst)

    _ -> t) ([]) players

splitCAPlayers :: [(ClientID,Player)] -> ([(ClientID,CPlayer)])
splitCAPlayers players = foldl (\t@(fromClst) pl -> case pl of
    (cId,p@(PCPlayer {})) -> ((cId,wrapCPlayer p):fromClst)

    _ -> t) ([]) players

splitABCPlayers :: [(ClientID,Player)] -> ([(ClientID,APlayer)],[(ClientID,BPlayer)])
splitABCPlayers players = foldl (\t@(fromAlst,fromBlst) pl -> case pl of
    (cId,p@(PAPlayer {})) -> ((cId,wrapAPlayer p):fromAlst,fromBlst)
    (cId,p@(PBPlayer {})) -> (fromAlst,(cId,wrapBPlayer p):fromBlst)

    _ -> t) ([],[]) players


-- process player connect
clientConnect :: FromSuperPlace -> ClientID -> NetState Player -> NetState Player
clientConnect fsp clientID state =
    let
        (a,aPlayer) = Update.clientConnect fsp clientID (fromJust $ TM.lookup $ placeStates state)
    in
        state { placeStates = TM.insert a $ placeStates state, playerStates = IM'.insert clientID (unwrapAPlayer aPlayer) (playerStates state) }

-- process player disconnects
disconnect :: FromSuperPlace -> ClientID -> NetState Player -> NetState Player
disconnect fsp clientID state =
    let
        player = fromJust $ IM'.lookup clientID $ players
        places = placeStates state
        players = playerStates state
        newPlaces = case player of
            PAPlayer {} -> (flip TM.insert) places $ clientDisconnectFromA fsp clientID (fromJust $ TM.lookup places) (wrapAPlayer player)
            PBPlayer {} -> (flip TM.insert) places $ clientDisconnectFromB fsp clientID (fromJust $ TM.lookup places) (wrapBPlayer player)
            PCPlayer {} -> (flip TM.insert) places $ clientDisconnectFromC fsp clientID (fromJust $ TM.lookup places) (wrapCPlayer player)

        newPlayers = IM'.delete clientID players
    in
        state { playerStates = newPlayers, placeStates = newPlaces }

update :: TopLevelData -> Maybe ClientID -> Transition -> NetState Player -> (NetState Player,[(ClientID,ClientMessage)],Maybe (Cmd Transition))
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
