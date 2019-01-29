module NewYouthHack.Static.Update where
import NewYouthHack.Static.Types
import NewYouthHack.Static.Wrappers
import NewYouthHack.Static.FromSuperPlace (FromSuperPlace(..))
import NewYouthHack.Update as Update
import qualified Data.TMap as TM
import Static.ServerTypes
import qualified Data.IntMap.Strict as IM'
import Data.Maybe (fromJust, isJust, mapMaybe)

-- player processing functions
processEnterMcMasterCreateNoticePlayer :: (McMasterUniversityPlayer -> EnterMcMasterCreateNoticefromMcMasterUniversity) -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))
processEnterMcMasterCreateNoticePlayer fromMcMasterUniversity (cId,player) = case player of
    (PMcMasterUniversityPlayer clientID)  -> let (np, mCm) = (unwrapEnterMcMasterCreateNoticefromMcMasterUniversity $ fromMcMasterUniversity $ wrapMcMasterUniversityPlayer player) in ((cId, np), (cId, mCm))


processCancelNoticePlayer :: (McMasterCreateNoticePlayer -> CancelNoticefromMcMasterCreateNotice) -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))
processCancelNoticePlayer fromMcMasterCreateNotice (cId,player) = case player of
    (PMcMasterCreateNoticePlayer clientID)  -> let (np, mCm) = (unwrapCancelNoticefromMcMasterCreateNotice $ fromMcMasterCreateNotice $ wrapMcMasterCreateNoticePlayer player) in ((cId, np), (cId, mCm))


processEnterUniversitiesPlayer :: (SquareOnePlayer -> EnterUniversitiesfromSquareOne) -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))
processEnterUniversitiesPlayer fromSquareOne (cId,player) = case player of
    PSquareOnePlayer  -> let (np, mCm) = (unwrapEnterUniversitiesfromSquareOne $ fromSquareOne $ wrapSquareOnePlayer player) in ((cId, np), (cId, mCm))


processExitUniversitiesPlayer :: (UniversitiesAndCollegesPlayer -> ExitUniversitiesfromUniversitiesAndColleges) -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))
processExitUniversitiesPlayer fromUniversitiesAndColleges (cId,player) = case player of
    (PUniversitiesAndCollegesPlayer clientID)  -> let (np, mCm) = (unwrapExitUniversitiesfromUniversitiesAndColleges $ fromUniversitiesAndColleges $ wrapUniversitiesAndCollegesPlayer player) in ((cId, np), (cId, mCm))


processEnterMcMasterUniversityPlayer :: (UniversitiesAndCollegesPlayer -> EnterMcMasterUniversityfromUniversitiesAndColleges) -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))
processEnterMcMasterUniversityPlayer fromUniversitiesAndColleges (cId,player) = case player of
    (PUniversitiesAndCollegesPlayer clientID)  -> let (np, mCm) = (unwrapEnterMcMasterUniversityfromUniversitiesAndColleges $ fromUniversitiesAndColleges $ wrapUniversitiesAndCollegesPlayer player) in ((cId, np), (cId, mCm))


processExitMcMasterUniversityPlayer :: (McMasterUniversityPlayer -> ExitMcMasterUniversityfromMcMasterUniversity) -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))
processExitMcMasterUniversityPlayer fromMcMasterUniversity (cId,player) = case player of
    (PMcMasterUniversityPlayer clientID)  -> let (np, mCm) = (unwrapExitMcMasterUniversityfromMcMasterUniversity $ fromMcMasterUniversity $ wrapMcMasterUniversityPlayer player) in ((cId, np), (cId, mCm))


processEditMcMasterCommentPlayer :: (McMasterUniversityPlayer -> EditMcMasterCommentfromMcMasterUniversity) -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))
processEditMcMasterCommentPlayer fromMcMasterUniversity (cId,player) = case player of
    (PMcMasterUniversityPlayer clientID)  -> let (np, mCm) = (unwrapEditMcMasterCommentfromMcMasterUniversity $ fromMcMasterUniversity $ wrapMcMasterUniversityPlayer player) in ((cId, np), (cId, mCm))


processSendMcMasterCommentPlayer :: (McMasterUniversityPlayer -> SendMcMasterCommentfromMcMasterUniversity) -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))
processSendMcMasterCommentPlayer fromMcMasterUniversity (cId,player) = case player of
    (PMcMasterUniversityPlayer clientID)  -> let (np, mCm) = (unwrapSendMcMasterCommentfromMcMasterUniversity $ fromMcMasterUniversity $ wrapMcMasterUniversityPlayer player) in ((cId, np), (cId, mCm))


processEditMcMasterNoticePlayer :: (McMasterCreateNoticePlayer -> EditMcMasterNoticefromMcMasterCreateNotice) -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))
processEditMcMasterNoticePlayer fromMcMasterCreateNotice (cId,player) = case player of
    (PMcMasterCreateNoticePlayer clientID)  -> let (np, mCm) = (unwrapEditMcMasterNoticefromMcMasterCreateNotice $ fromMcMasterCreateNotice $ wrapMcMasterCreateNoticePlayer player) in ((cId, np), (cId, mCm))


processPublishMcMasterNoticePlayer :: (McMasterCreateNoticePlayer -> PublishMcMasterNoticefromMcMasterCreateNotice) -> (McMasterUniversityPlayer -> PublishMcMasterNoticefromMcMasterUniversity) -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))
processPublishMcMasterNoticePlayer fromMcMasterCreateNotice fromMcMasterUniversity (cId,player) = case player of
    (PMcMasterCreateNoticePlayer clientID)  -> let (np, mCm) = (unwrapPublishMcMasterNoticefromMcMasterCreateNotice $ fromMcMasterCreateNotice $ wrapMcMasterCreateNoticePlayer player) in ((cId, np), (cId, mCm))
    (PMcMasterUniversityPlayer clientID)  -> let (np, mCm) = (unwrapPublishMcMasterNoticefromMcMasterUniversity $ fromMcMasterUniversity $ wrapMcMasterUniversityPlayer player) in ((cId, np), (cId, mCm))



-- player splitting functions
splitEnterMcMasterCreateNoticePlayers :: [(ClientID,Player)] -> ([(ClientID,McMasterUniversityPlayer)])
splitEnterMcMasterCreateNoticePlayers players = foldl (\t@(fromMcMasterUniversitylst) pl -> case pl of
    (cId,p@(PMcMasterUniversityPlayer {})) -> ((cId,wrapMcMasterUniversityPlayer p):fromMcMasterUniversitylst)

    _ -> t) ([]) players

splitCancelNoticePlayers :: [(ClientID,Player)] -> ([(ClientID,McMasterCreateNoticePlayer)])
splitCancelNoticePlayers players = foldl (\t@(fromMcMasterCreateNoticelst) pl -> case pl of
    (cId,p@(PMcMasterCreateNoticePlayer {})) -> ((cId,wrapMcMasterCreateNoticePlayer p):fromMcMasterCreateNoticelst)

    _ -> t) ([]) players

splitEnterUniversitiesPlayers :: [(ClientID,Player)] -> ([(ClientID,SquareOnePlayer)])
splitEnterUniversitiesPlayers players = foldl (\t@(fromSquareOnelst) pl -> case pl of
    (cId,p@(PSquareOnePlayer {})) -> ((cId,wrapSquareOnePlayer p):fromSquareOnelst)

    _ -> t) ([]) players

splitExitUniversitiesPlayers :: [(ClientID,Player)] -> ([(ClientID,UniversitiesAndCollegesPlayer)])
splitExitUniversitiesPlayers players = foldl (\t@(fromUniversitiesAndCollegeslst) pl -> case pl of
    (cId,p@(PUniversitiesAndCollegesPlayer {})) -> ((cId,wrapUniversitiesAndCollegesPlayer p):fromUniversitiesAndCollegeslst)

    _ -> t) ([]) players

splitEnterMcMasterUniversityPlayers :: [(ClientID,Player)] -> ([(ClientID,UniversitiesAndCollegesPlayer)])
splitEnterMcMasterUniversityPlayers players = foldl (\t@(fromUniversitiesAndCollegeslst) pl -> case pl of
    (cId,p@(PUniversitiesAndCollegesPlayer {})) -> ((cId,wrapUniversitiesAndCollegesPlayer p):fromUniversitiesAndCollegeslst)

    _ -> t) ([]) players

splitExitMcMasterUniversityPlayers :: [(ClientID,Player)] -> ([(ClientID,McMasterUniversityPlayer)])
splitExitMcMasterUniversityPlayers players = foldl (\t@(fromMcMasterUniversitylst) pl -> case pl of
    (cId,p@(PMcMasterUniversityPlayer {})) -> ((cId,wrapMcMasterUniversityPlayer p):fromMcMasterUniversitylst)

    _ -> t) ([]) players

splitEditMcMasterCommentPlayers :: [(ClientID,Player)] -> ([(ClientID,McMasterUniversityPlayer)])
splitEditMcMasterCommentPlayers players = foldl (\t@(fromMcMasterUniversitylst) pl -> case pl of
    (cId,p@(PMcMasterUniversityPlayer {})) -> ((cId,wrapMcMasterUniversityPlayer p):fromMcMasterUniversitylst)

    _ -> t) ([]) players

splitSendMcMasterCommentPlayers :: [(ClientID,Player)] -> ([(ClientID,McMasterUniversityPlayer)])
splitSendMcMasterCommentPlayers players = foldl (\t@(fromMcMasterUniversitylst) pl -> case pl of
    (cId,p@(PMcMasterUniversityPlayer {})) -> ((cId,wrapMcMasterUniversityPlayer p):fromMcMasterUniversitylst)

    _ -> t) ([]) players

splitEditMcMasterNoticePlayers :: [(ClientID,Player)] -> ([(ClientID,McMasterCreateNoticePlayer)])
splitEditMcMasterNoticePlayers players = foldl (\t@(fromMcMasterCreateNoticelst) pl -> case pl of
    (cId,p@(PMcMasterCreateNoticePlayer {})) -> ((cId,wrapMcMasterCreateNoticePlayer p):fromMcMasterCreateNoticelst)

    _ -> t) ([]) players

splitPublishMcMasterNoticePlayers :: [(ClientID,Player)] -> ([(ClientID,McMasterCreateNoticePlayer)],[(ClientID,McMasterUniversityPlayer)])
splitPublishMcMasterNoticePlayers players = foldl (\t@(fromMcMasterCreateNoticelst,fromMcMasterUniversitylst) pl -> case pl of
    (cId,p@(PMcMasterCreateNoticePlayer {})) -> ((cId,wrapMcMasterCreateNoticePlayer p):fromMcMasterCreateNoticelst,fromMcMasterUniversitylst)
    (cId,p@(PMcMasterUniversityPlayer {})) -> (fromMcMasterCreateNoticelst,(cId,wrapMcMasterUniversityPlayer p):fromMcMasterUniversitylst)

    _ -> t) ([],[]) players


-- process player connect
clientConnect :: FromSuperPlace -> ClientID -> NetState Player -> NetState Player
clientConnect fsp clientID state =
    let
        (squareOne,squareOnePlayer) = Update.clientConnect fsp clientID (fromJust $ TM.lookup $ placeStates state)
    in
        state { placeStates = TM.insert squareOne $ placeStates state, playerStates = IM'.insert clientID (unwrapSquareOnePlayer squareOnePlayer) (playerStates state) }

-- process player disconnects
disconnect :: FromSuperPlace -> ClientID -> NetState Player -> NetState Player
disconnect fsp clientID state =
    let
        player = fromJust $ IM'.lookup clientID $ players
        places = placeStates state
        players = playerStates state
        newPlaces = case player of
            PSquareOnePlayer {} -> (flip TM.insert) places $ clientDisconnectFromSquareOne fsp clientID (fromJust $ TM.lookup places) (wrapSquareOnePlayer player)
            PUniversitiesAndCollegesPlayer {} -> (flip TM.insert) places $ clientDisconnectFromUniversitiesAndColleges fsp clientID (fromJust $ TM.lookup places) (wrapUniversitiesAndCollegesPlayer player)
            PMcMasterUniversityPlayer {} -> (flip TM.insert) places $ clientDisconnectFromMcMasterUniversity fsp clientID (fromJust $ TM.lookup places) (wrapMcMasterUniversityPlayer player)
            PMcMasterCreateNoticePlayer {} -> (flip TM.insert) places $ clientDisconnectFromMcMasterCreateNotice fsp clientID (fromJust $ TM.lookup places) (wrapMcMasterCreateNoticePlayer player)

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
                TEnterMcMasterCreateNotice ->
                    let
                        (mcMasterUniversityPlayerLst) = splitEnterMcMasterCreateNoticePlayers (IM'.toList players)
                        (mcMasterUniversity,mcMasterCreateNotice,fromMcMasterUniversity) = updateEnterMcMasterCreateNotice tld (fromJust mClientID) (wrapEnterMcMasterCreateNotice trans) (fromJust $ TM.lookup places) (fromJust $ TM.lookup places) (map snd mcMasterUniversityPlayerLst)
                        newPlaces = TM.insert mcMasterUniversity $ TM.insert mcMasterCreateNotice places
                        (newPlayers, clientMessages) = unzip $ map (processEnterMcMasterCreateNoticePlayer fromMcMasterUniversity) (IM'.toList players)
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)

                TCancelNotice ->
                    let
                        (mcMasterCreateNoticePlayerLst) = splitCancelNoticePlayers (IM'.toList players)
                        (mcMasterCreateNotice,mcMasterUniversity,fromMcMasterCreateNotice) = updateCancelNotice tld (fromJust mClientID) (wrapCancelNotice trans) (fromJust $ TM.lookup places) (fromJust $ TM.lookup places) (map snd mcMasterCreateNoticePlayerLst)
                        newPlaces = TM.insert mcMasterCreateNotice $ TM.insert mcMasterUniversity places
                        (newPlayers, clientMessages) = unzip $ map (processCancelNoticePlayer fromMcMasterCreateNotice) (IM'.toList players)
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)

                TEnterUniversities ->
                    let
                        (squareOnePlayerLst) = splitEnterUniversitiesPlayers (IM'.toList players)
                        (squareOne,universitiesAndColleges,fromSquareOne) = updateEnterUniversities tld (fromJust mClientID) (wrapEnterUniversities trans) (fromJust $ TM.lookup places) (fromJust $ TM.lookup places) (map snd squareOnePlayerLst)
                        newPlaces = TM.insert squareOne $ TM.insert universitiesAndColleges places
                        (newPlayers, clientMessages) = unzip $ map (processEnterUniversitiesPlayer fromSquareOne) (IM'.toList players)
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)

                TExitUniversities ->
                    let
                        (universitiesAndCollegesPlayerLst) = splitExitUniversitiesPlayers (IM'.toList players)
                        (universitiesAndColleges,squareOne,fromUniversitiesAndColleges) = updateExitUniversities tld (fromJust mClientID) (wrapExitUniversities trans) (fromJust $ TM.lookup places) (fromJust $ TM.lookup places) (map snd universitiesAndCollegesPlayerLst)
                        newPlaces = TM.insert universitiesAndColleges $ TM.insert squareOne places
                        (newPlayers, clientMessages) = unzip $ map (processExitUniversitiesPlayer fromUniversitiesAndColleges) (IM'.toList players)
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)

                TEnterMcMasterUniversity ->
                    let
                        (universitiesAndCollegesPlayerLst) = splitEnterMcMasterUniversityPlayers (IM'.toList players)
                        (universitiesAndColleges,mcMasterUniversity,fromUniversitiesAndColleges) = updateEnterMcMasterUniversity tld (fromJust mClientID) (wrapEnterMcMasterUniversity trans) (fromJust $ TM.lookup places) (fromJust $ TM.lookup places) (map snd universitiesAndCollegesPlayerLst)
                        newPlaces = TM.insert universitiesAndColleges $ TM.insert mcMasterUniversity places
                        (newPlayers, clientMessages) = unzip $ map (processEnterMcMasterUniversityPlayer fromUniversitiesAndColleges) (IM'.toList players)
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)

                TExitMcMasterUniversity ->
                    let
                        (mcMasterUniversityPlayerLst) = splitExitMcMasterUniversityPlayers (IM'.toList players)
                        (mcMasterUniversity,universitiesAndColleges,fromMcMasterUniversity) = updateExitMcMasterUniversity tld (fromJust mClientID) (wrapExitMcMasterUniversity trans) (fromJust $ TM.lookup places) (fromJust $ TM.lookup places) (map snd mcMasterUniversityPlayerLst)
                        newPlaces = TM.insert mcMasterUniversity $ TM.insert universitiesAndColleges places
                        (newPlayers, clientMessages) = unzip $ map (processExitMcMasterUniversityPlayer fromMcMasterUniversity) (IM'.toList players)
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)

                (TEditMcMasterComment partialComment) ->
                    let
                        (mcMasterUniversityPlayerLst) = splitEditMcMasterCommentPlayers (IM'.toList players)
                        (mcMasterUniversity,fromMcMasterUniversity) = updateEditMcMasterComment tld (fromJust mClientID) (wrapEditMcMasterComment trans) (fromJust $ TM.lookup places) (map snd mcMasterUniversityPlayerLst)
                        newPlaces = TM.insert mcMasterUniversity places
                        (newPlayers, clientMessages) = unzip $ map (processEditMcMasterCommentPlayer fromMcMasterUniversity) (IM'.toList players)
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)

                (TSendMcMasterComment comment) ->
                    let
                        (mcMasterUniversityPlayerLst) = splitSendMcMasterCommentPlayers (IM'.toList players)
                        (mcMasterUniversity,fromMcMasterUniversity) = updateSendMcMasterComment tld (fromJust mClientID) (wrapSendMcMasterComment trans) (fromJust $ TM.lookup places) (map snd mcMasterUniversityPlayerLst)
                        newPlaces = TM.insert mcMasterUniversity places
                        (newPlayers, clientMessages) = unzip $ map (processSendMcMasterCommentPlayer fromMcMasterUniversity) (IM'.toList players)
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)

                (TEditMcMasterNotice partialNotice) ->
                    let
                        (mcMasterCreateNoticePlayerLst) = splitEditMcMasterNoticePlayers (IM'.toList players)
                        (mcMasterCreateNotice,fromMcMasterCreateNotice) = updateEditMcMasterNotice tld (fromJust mClientID) (wrapEditMcMasterNotice trans) (fromJust $ TM.lookup places) (map snd mcMasterCreateNoticePlayerLst)
                        newPlaces = TM.insert mcMasterCreateNotice places
                        (newPlayers, clientMessages) = unzip $ map (processEditMcMasterNoticePlayer fromMcMasterCreateNotice) (IM'.toList players)
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)

                (TPublishMcMasterNotice notice) ->
                    let
                        (mcMasterCreateNoticePlayerLst,mcMasterUniversityPlayerLst) = splitPublishMcMasterNoticePlayers (IM'.toList players)
                        (mcMasterCreateNotice,mcMasterUniversity,fromMcMasterCreateNotice,fromMcMasterUniversity) = updatePublishMcMasterNotice tld (fromJust mClientID) (wrapPublishMcMasterNotice trans) (fromJust $ TM.lookup places) (fromJust $ TM.lookup places) (map snd mcMasterCreateNoticePlayerLst) (map snd mcMasterUniversityPlayerLst)
                        newPlaces = TM.insert mcMasterCreateNotice $ TM.insert mcMasterUniversity places
                        (newPlayers, clientMessages) = unzip $ map (processPublishMcMasterNoticePlayer fromMcMasterCreateNotice fromMcMasterUniversity) (IM'.toList players)
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
