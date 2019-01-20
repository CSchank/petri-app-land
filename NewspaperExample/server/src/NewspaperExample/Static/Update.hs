module NewspaperExample.Static.Update where
import NewspaperExample.Static.Types
import NewspaperExample.Static.Wrappers
import NewspaperExample.Static.FromSuperPlace (FromSuperPlace(..))
import NewspaperExample.Update as Update
import qualified Data.TMap as TM
import Static.ServerTypes
import qualified Data.IntMap.Strict as IM'
import Data.Maybe (fromJust, isJust, mapMaybe)

-- player processing functions
processEnterReadingRoomPlayer :: (MainStreetPlayer -> EnterReadingRoomfromMainStreet) -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))
processEnterReadingRoomPlayer fromMainStreet (cId,player) = case player of
    PMainStreetPlayer  -> let (np, mCm) = (unwrapEnterReadingRoomfromMainStreet $ fromMainStreet $ wrapMainStreetPlayer player) in ((cId, np), (cId, mCm))


processEnterEditingRoomPlayer :: (MainStreetPlayer -> EnterEditingRoomfromMainStreet) -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))
processEnterEditingRoomPlayer fromMainStreet (cId,player) = case player of
    PMainStreetPlayer  -> let (np, mCm) = (unwrapEnterEditingRoomfromMainStreet $ fromMainStreet $ wrapMainStreetPlayer player) in ((cId, np), (cId, mCm))


processStartEditingPlayer :: (EditingRoomPlayer -> StartEditingfromEditingRoom) -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))
processStartEditingPlayer fromEditingRoom (cId,player) = case player of
    (PEditingRoomPlayer maybeEditing)  -> let (np, mCm) = (unwrapStartEditingfromEditingRoom $ fromEditingRoom $ wrapEditingRoomPlayer player) in ((cId, np), (cId, mCm))


processLeaveReadingRoomPlayer :: (ReadingRoomPlayer -> LeaveReadingRoomfromReadingRoom) -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))
processLeaveReadingRoomPlayer fromReadingRoom (cId,player) = case player of
    (PReadingRoomPlayer nowReading)  -> let (np, mCm) = (unwrapLeaveReadingRoomfromReadingRoom $ fromReadingRoom $ wrapReadingRoomPlayer player) in ((cId, np), (cId, mCm))


processLeaveEditingRoomPlayer :: (EditingRoomPlayer -> LeaveEditingRoomfromEditingRoom) -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))
processLeaveEditingRoomPlayer fromEditingRoom (cId,player) = case player of
    (PEditingRoomPlayer maybeEditing)  -> let (np, mCm) = (unwrapLeaveEditingRoomfromEditingRoom $ fromEditingRoom $ wrapEditingRoomPlayer player) in ((cId, np), (cId, mCm))


processPublishArticlePlayer :: (EditingRoomPlayer -> PublishArticlefromEditingRoom) -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))
processPublishArticlePlayer fromEditingRoom (cId,player) = case player of
    (PEditingRoomPlayer maybeEditing)  -> let (np, mCm) = (unwrapPublishArticlefromEditingRoom $ fromEditingRoom $ wrapEditingRoomPlayer player) in ((cId, np), (cId, mCm))


processSaveDraftPlayer :: (EditingRoomPlayer -> SaveDraftfromEditingRoom) -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))
processSaveDraftPlayer fromEditingRoom (cId,player) = case player of
    (PEditingRoomPlayer maybeEditing)  -> let (np, mCm) = (unwrapSaveDraftfromEditingRoom $ fromEditingRoom $ wrapEditingRoomPlayer player) in ((cId, np), (cId, mCm))


processEnterTitlePlayer :: (EditingRoomPlayer -> EnterTitlefromEditingRoom) -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))
processEnterTitlePlayer fromEditingRoom (cId,player) = case player of
    (PEditingRoomPlayer maybeEditing)  -> let (np, mCm) = (unwrapEnterTitlefromEditingRoom $ fromEditingRoom $ wrapEditingRoomPlayer player) in ((cId, np), (cId, mCm))


processEnterTextPlayer :: (EditingRoomPlayer -> EnterTextfromEditingRoom) -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))
processEnterTextPlayer fromEditingRoom (cId,player) = case player of
    (PEditingRoomPlayer maybeEditing)  -> let (np, mCm) = (unwrapEnterTextfromEditingRoom $ fromEditingRoom $ wrapEditingRoomPlayer player) in ((cId, np), (cId, mCm))


processEnterCommentPlayer :: (EditingRoomPlayer -> EnterCommentfromEditingRoom) -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))
processEnterCommentPlayer fromEditingRoom (cId,player) = case player of
    (PEditingRoomPlayer maybeEditing)  -> let (np, mCm) = (unwrapEnterCommentfromEditingRoom $ fromEditingRoom $ wrapEditingRoomPlayer player) in ((cId, np), (cId, mCm))


processPostCommentPlayer :: (EditingRoomPlayer -> PostCommentfromEditingRoom) -> (ClientID, Player) -> ((ClientID, Player), (ClientID, Maybe ClientMessage))
processPostCommentPlayer fromEditingRoom (cId,player) = case player of
    (PEditingRoomPlayer maybeEditing)  -> let (np, mCm) = (unwrapPostCommentfromEditingRoom $ fromEditingRoom $ wrapEditingRoomPlayer player) in ((cId, np), (cId, mCm))



-- player splitting functions
splitEnterReadingRoomPlayers :: [(ClientID,Player)] -> ([(ClientID,MainStreetPlayer)])
splitEnterReadingRoomPlayers players = foldl (\t@(fromMainStreetlst) pl -> case pl of
    (cId,p@(PMainStreetPlayer {})) -> ((cId,wrapMainStreetPlayer p):fromMainStreetlst)

    _ -> t) ([]) players

splitEnterEditingRoomPlayers :: [(ClientID,Player)] -> ([(ClientID,MainStreetPlayer)])
splitEnterEditingRoomPlayers players = foldl (\t@(fromMainStreetlst) pl -> case pl of
    (cId,p@(PMainStreetPlayer {})) -> ((cId,wrapMainStreetPlayer p):fromMainStreetlst)

    _ -> t) ([]) players

splitStartEditingPlayers :: [(ClientID,Player)] -> ([(ClientID,EditingRoomPlayer)])
splitStartEditingPlayers players = foldl (\t@(fromEditingRoomlst) pl -> case pl of
    (cId,p@(PEditingRoomPlayer {})) -> ((cId,wrapEditingRoomPlayer p):fromEditingRoomlst)

    _ -> t) ([]) players

splitLeaveReadingRoomPlayers :: [(ClientID,Player)] -> ([(ClientID,ReadingRoomPlayer)])
splitLeaveReadingRoomPlayers players = foldl (\t@(fromReadingRoomlst) pl -> case pl of
    (cId,p@(PReadingRoomPlayer {})) -> ((cId,wrapReadingRoomPlayer p):fromReadingRoomlst)

    _ -> t) ([]) players

splitLeaveEditingRoomPlayers :: [(ClientID,Player)] -> ([(ClientID,EditingRoomPlayer)])
splitLeaveEditingRoomPlayers players = foldl (\t@(fromEditingRoomlst) pl -> case pl of
    (cId,p@(PEditingRoomPlayer {})) -> ((cId,wrapEditingRoomPlayer p):fromEditingRoomlst)

    _ -> t) ([]) players

splitPublishArticlePlayers :: [(ClientID,Player)] -> ([(ClientID,EditingRoomPlayer)])
splitPublishArticlePlayers players = foldl (\t@(fromEditingRoomlst) pl -> case pl of
    (cId,p@(PEditingRoomPlayer {})) -> ((cId,wrapEditingRoomPlayer p):fromEditingRoomlst)

    _ -> t) ([]) players

splitSaveDraftPlayers :: [(ClientID,Player)] -> ([(ClientID,EditingRoomPlayer)])
splitSaveDraftPlayers players = foldl (\t@(fromEditingRoomlst) pl -> case pl of
    (cId,p@(PEditingRoomPlayer {})) -> ((cId,wrapEditingRoomPlayer p):fromEditingRoomlst)

    _ -> t) ([]) players

splitEnterTitlePlayers :: [(ClientID,Player)] -> ([(ClientID,EditingRoomPlayer)])
splitEnterTitlePlayers players = foldl (\t@(fromEditingRoomlst) pl -> case pl of
    (cId,p@(PEditingRoomPlayer {})) -> ((cId,wrapEditingRoomPlayer p):fromEditingRoomlst)

    _ -> t) ([]) players

splitEnterTextPlayers :: [(ClientID,Player)] -> ([(ClientID,EditingRoomPlayer)])
splitEnterTextPlayers players = foldl (\t@(fromEditingRoomlst) pl -> case pl of
    (cId,p@(PEditingRoomPlayer {})) -> ((cId,wrapEditingRoomPlayer p):fromEditingRoomlst)

    _ -> t) ([]) players

splitEnterCommentPlayers :: [(ClientID,Player)] -> ([(ClientID,EditingRoomPlayer)])
splitEnterCommentPlayers players = foldl (\t@(fromEditingRoomlst) pl -> case pl of
    (cId,p@(PEditingRoomPlayer {})) -> ((cId,wrapEditingRoomPlayer p):fromEditingRoomlst)

    _ -> t) ([]) players

splitPostCommentPlayers :: [(ClientID,Player)] -> ([(ClientID,EditingRoomPlayer)])
splitPostCommentPlayers players = foldl (\t@(fromEditingRoomlst) pl -> case pl of
    (cId,p@(PEditingRoomPlayer {})) -> ((cId,wrapEditingRoomPlayer p):fromEditingRoomlst)

    _ -> t) ([]) players


-- process player connect
clientConnect :: FromSuperPlace -> ClientID -> NetState Player -> NetState Player
clientConnect fsp clientID state =
    let
        (mainStreet,mainStreetPlayer) = Update.clientConnect fsp clientID (fromJust $ TM.lookup $ placeStates state)
    in
        state { placeStates = TM.insert mainStreet $ placeStates state, playerStates = IM'.insert clientID (unwrapMainStreetPlayer mainStreetPlayer) (playerStates state) }

-- process player disconnects
disconnect :: FromSuperPlace -> ClientID -> NetState Player -> NetState Player
disconnect fsp clientID state =
    let
        player = fromJust $ IM'.lookup clientID $ players
        places = placeStates state
        players = playerStates state
        newPlaces = case player of
            PMainStreetPlayer {} -> (flip TM.insert) places $ clientDisconnectFromMainStreet fsp clientID (fromJust $ TM.lookup places) (wrapMainStreetPlayer player)
            PReadingRoomPlayer {} -> (flip TM.insert) places $ clientDisconnectFromReadingRoom fsp clientID (fromJust $ TM.lookup places) (wrapReadingRoomPlayer player)
            PEditingRoomPlayer {} -> (flip TM.insert) places $ clientDisconnectFromEditingRoom fsp clientID (fromJust $ TM.lookup places) (wrapEditingRoomPlayer player)

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
                TEnterReadingRoom ->
                    let
                        (mainStreetPlayerLst) = splitEnterReadingRoomPlayers (IM'.toList players)
                        (mainStreet,readingRoom,fromMainStreet) = updateEnterReadingRoom tld mClientID (wrapEnterReadingRoom trans) (fromJust $ TM.lookup places) (fromJust $ TM.lookup places) (map snd mainStreetPlayerLst)
                        newPlaces = TM.insert mainStreet $ TM.insert readingRoom places
                        (newPlayers, clientMessages) = unzip $ map (processEnterReadingRoomPlayer fromMainStreet) (IM'.toList players)
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)

                TEnterEditingRoom ->
                    let
                        (mainStreetPlayerLst) = splitEnterEditingRoomPlayers (IM'.toList players)
                        (mainStreet,readingRoom,fromMainStreet) = updateEnterEditingRoom tld mClientID (wrapEnterEditingRoom trans) (fromJust $ TM.lookup places) (fromJust $ TM.lookup places) (map snd mainStreetPlayerLst)
                        newPlaces = TM.insert mainStreet $ TM.insert readingRoom places
                        (newPlayers, clientMessages) = unzip $ map (processEnterEditingRoomPlayer fromMainStreet) (IM'.toList players)
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)

                (TStartEditing title) ->
                    let
                        (editingRoomPlayerLst) = splitStartEditingPlayers (IM'.toList players)
                        (editingRoom,fromEditingRoom) = updateStartEditing tld mClientID (wrapStartEditing trans) (fromJust $ TM.lookup places) (map snd editingRoomPlayerLst)
                        newPlaces = TM.insert editingRoom places
                        (newPlayers, clientMessages) = unzip $ map (processStartEditingPlayer fromEditingRoom) (IM'.toList players)
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)

                TLeaveReadingRoom ->
                    let
                        (readingRoomPlayerLst) = splitLeaveReadingRoomPlayers (IM'.toList players)
                        (readingRoom,mainStreet,fromReadingRoom) = updateLeaveReadingRoom tld mClientID (wrapLeaveReadingRoom trans) (fromJust $ TM.lookup places) (fromJust $ TM.lookup places) (map snd readingRoomPlayerLst)
                        newPlaces = TM.insert readingRoom $ TM.insert mainStreet places
                        (newPlayers, clientMessages) = unzip $ map (processLeaveReadingRoomPlayer fromReadingRoom) (IM'.toList players)
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)

                TLeaveEditingRoom ->
                    let
                        (editingRoomPlayerLst) = splitLeaveEditingRoomPlayers (IM'.toList players)
                        (editingRoom,mainStreet,fromEditingRoom) = updateLeaveEditingRoom tld mClientID (wrapLeaveEditingRoom trans) (fromJust $ TM.lookup places) (fromJust $ TM.lookup places) (map snd editingRoomPlayerLst)
                        newPlaces = TM.insert editingRoom $ TM.insert mainStreet places
                        (newPlayers, clientMessages) = unzip $ map (processLeaveEditingRoomPlayer fromEditingRoom) (IM'.toList players)
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)

                TPublishArticle ->
                    let
                        (editingRoomPlayerLst) = splitPublishArticlePlayers (IM'.toList players)
                        (editingRoom,readingRoom,fromEditingRoom) = updatePublishArticle tld mClientID (wrapPublishArticle trans) (fromJust $ TM.lookup places) (fromJust $ TM.lookup places) (map snd editingRoomPlayerLst)
                        newPlaces = TM.insert editingRoom $ TM.insert readingRoom places
                        (newPlayers, clientMessages) = unzip $ map (processPublishArticlePlayer fromEditingRoom) (IM'.toList players)
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)

                (TSaveDraft draft) ->
                    let
                        (editingRoomPlayerLst) = splitSaveDraftPlayers (IM'.toList players)
                        (editingRoom,fromEditingRoom) = updateSaveDraft tld mClientID (wrapSaveDraft trans) (fromJust $ TM.lookup places) (map snd editingRoomPlayerLst)
                        newPlaces = TM.insert editingRoom places
                        (newPlayers, clientMessages) = unzip $ map (processSaveDraftPlayer fromEditingRoom) (IM'.toList players)
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)

                (TEnterTitle title) ->
                    let
                        (editingRoomPlayerLst) = splitEnterTitlePlayers (IM'.toList players)
                        (editingRoom,fromEditingRoom) = updateEnterTitle tld mClientID (wrapEnterTitle trans) (fromJust $ TM.lookup places) (map snd editingRoomPlayerLst)
                        newPlaces = TM.insert editingRoom places
                        (newPlayers, clientMessages) = unzip $ map (processEnterTitlePlayer fromEditingRoom) (IM'.toList players)
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)

                (TEnterText text) ->
                    let
                        (editingRoomPlayerLst) = splitEnterTextPlayers (IM'.toList players)
                        (editingRoom,fromEditingRoom) = updateEnterText tld mClientID (wrapEnterText trans) (fromJust $ TM.lookup places) (map snd editingRoomPlayerLst)
                        newPlaces = TM.insert editingRoom places
                        (newPlayers, clientMessages) = unzip $ map (processEnterTextPlayer fromEditingRoom) (IM'.toList players)
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)

                (TEnterComment comment) ->
                    let
                        (editingRoomPlayerLst) = splitEnterCommentPlayers (IM'.toList players)
                        (editingRoom,fromEditingRoom) = updateEnterComment tld mClientID (wrapEnterComment trans) (fromJust $ TM.lookup places) (map snd editingRoomPlayerLst)
                        newPlaces = TM.insert editingRoom places
                        (newPlayers, clientMessages) = unzip $ map (processEnterCommentPlayer fromEditingRoom) (IM'.toList players)
                    in
                        (newPlaces, newPlayers, clientMessages, Nothing)

                (TPostComment comment) ->
                    let
                        (editingRoomPlayerLst) = splitPostCommentPlayers (IM'.toList players)
                        (editingRoom,fromEditingRoom) = updatePostComment tld mClientID (wrapPostComment trans) (fromJust $ TM.lookup places) (map snd editingRoomPlayerLst)
                        newPlaces = TM.insert editingRoom places
                        (newPlayers, clientMessages) = unzip $ map (processPostCommentPlayer fromEditingRoom) (IM'.toList players)
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
