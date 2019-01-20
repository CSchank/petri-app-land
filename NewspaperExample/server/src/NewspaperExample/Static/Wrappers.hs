module NewspaperExample.Static.Wrappers where
import NewspaperExample.Static.Types

unwrapDidEnterReadingRoom :: DidEnterReadingRoom -> ClientMessage
unwrapDidEnterReadingRoom (DidEnterReadingRoom articles)  = (MDidEnterReadingRoom articles) 


unwrapDidEnterEditingRoom :: DidEnterEditingRoom -> ClientMessage
unwrapDidEnterEditingRoom (DidEnterEditingRoom articles)  = (MDidEnterEditingRoom articles) 


unwrapDidStartEditing :: DidStartEditing -> ClientMessage
unwrapDidStartEditing (DidStartEditing draft)  = (MDidStartEditing draft) 


unwrapDidLeaveReadingRoom :: DidLeaveReadingRoom -> ClientMessage
unwrapDidLeaveReadingRoom DidLeaveReadingRoom  = MDidLeaveReadingRoom 


unwrapDidLeaveEditingRoom :: DidLeaveEditingRoom -> ClientMessage
unwrapDidLeaveEditingRoom DidLeaveEditingRoom  = MDidLeaveEditingRoom 


unwrapDidPublish :: DidPublish -> ClientMessage
unwrapDidPublish (DidPublish articles)  = (MDidPublish articles) 


unwrapDidSaveDraft :: DidSaveDraft -> ClientMessage
unwrapDidSaveDraft (DidSaveDraft articles)  = (MDidSaveDraft articles) 


unwrapDidEnterTitle :: DidEnterTitle -> ClientMessage
unwrapDidEnterTitle (DidEnterTitle articles)  = (MDidEnterTitle articles) 


unwrapDidEnterText :: DidEnterText -> ClientMessage
unwrapDidEnterText (DidEnterText articles)  = (MDidEnterText articles) 


unwrapDidEnterComment :: DidEnterComment -> ClientMessage
unwrapDidEnterComment (DidEnterComment comment)  = (MDidEnterComment comment) 


unwrapDidPostComment :: DidPostComment -> ClientMessage
unwrapDidPostComment (DidPostComment comment)  = (MDidPostComment comment) 



wrapDidEnterReadingRoom :: ClientMessage -> DidEnterReadingRoom
wrapDidEnterReadingRoom x__ =
    case x__ of
        (MDidEnterReadingRoom articles)  -> (DidEnterReadingRoom articles) 
        _ -> DidEnterReadingRoom []


wrapDidEnterEditingRoom :: ClientMessage -> DidEnterEditingRoom
wrapDidEnterEditingRoom x__ =
    case x__ of
        (MDidEnterEditingRoom articles)  -> (DidEnterEditingRoom articles) 
        _ -> DidEnterEditingRoom []


wrapDidStartEditing :: ClientMessage -> DidStartEditing
wrapDidStartEditing x__ =
    case x__ of
        (MDidStartEditing draft)  -> (DidStartEditing draft) 
        _ -> DidStartEditing (DraftArticle "" "" 0 "" [])


wrapDidLeaveReadingRoom :: ClientMessage -> DidLeaveReadingRoom
wrapDidLeaveReadingRoom x__ =
    case x__ of
        MDidLeaveReadingRoom  -> DidLeaveReadingRoom 
        _ -> DidLeaveReadingRoom 


wrapDidLeaveEditingRoom :: ClientMessage -> DidLeaveEditingRoom
wrapDidLeaveEditingRoom x__ =
    case x__ of
        MDidLeaveEditingRoom  -> DidLeaveEditingRoom 
        _ -> DidLeaveEditingRoom 


wrapDidPublish :: ClientMessage -> DidPublish
wrapDidPublish x__ =
    case x__ of
        (MDidPublish articles)  -> (DidPublish articles) 
        _ -> DidPublish []


wrapDidSaveDraft :: ClientMessage -> DidSaveDraft
wrapDidSaveDraft x__ =
    case x__ of
        (MDidSaveDraft articles)  -> (DidSaveDraft articles) 
        _ -> DidSaveDraft []


wrapDidEnterTitle :: ClientMessage -> DidEnterTitle
wrapDidEnterTitle x__ =
    case x__ of
        (MDidEnterTitle articles)  -> (DidEnterTitle articles) 
        _ -> DidEnterTitle []


wrapDidEnterText :: ClientMessage -> DidEnterText
wrapDidEnterText x__ =
    case x__ of
        (MDidEnterText articles)  -> (DidEnterText articles) 
        _ -> DidEnterText []


wrapDidEnterComment :: ClientMessage -> DidEnterComment
wrapDidEnterComment x__ =
    case x__ of
        (MDidEnterComment comment)  -> (DidEnterComment comment) 
        _ -> DidEnterComment ""


wrapDidPostComment :: ClientMessage -> DidPostComment
wrapDidPostComment x__ =
    case x__ of
        (MDidPostComment comment)  -> (DidPostComment comment) 
        _ -> DidPostComment ""



unwrapMainStreetPlayer :: MainStreetPlayer -> Player
unwrapMainStreetPlayer MainStreetPlayer  = PMainStreetPlayer 


unwrapReadingRoomPlayer :: ReadingRoomPlayer -> Player
unwrapReadingRoomPlayer (ReadingRoomPlayer nowReading)  = (PReadingRoomPlayer nowReading) 


unwrapEditingRoomPlayer :: EditingRoomPlayer -> Player
unwrapEditingRoomPlayer (EditingRoomPlayer maybeEditing)  = (PEditingRoomPlayer maybeEditing) 



wrapMainStreetPlayer :: Player -> MainStreetPlayer
wrapMainStreetPlayer x__ =
    case x__ of
        PMainStreetPlayer  -> MainStreetPlayer 
        _ -> MainStreetPlayer 


wrapReadingRoomPlayer :: Player -> ReadingRoomPlayer
wrapReadingRoomPlayer x__ =
    case x__ of
        (PReadingRoomPlayer nowReading)  -> (ReadingRoomPlayer nowReading) 
        _ -> ReadingRoomPlayer ""


wrapEditingRoomPlayer :: Player -> EditingRoomPlayer
wrapEditingRoomPlayer x__ =
    case x__ of
        (PEditingRoomPlayer maybeEditing)  -> (EditingRoomPlayer maybeEditing) 
        _ -> EditingRoomPlayer Nothing



unwrapEnterReadingRoomfromMainStreet :: EnterReadingRoomfromMainStreet -> (Player, Maybe ClientMessage)
unwrapEnterReadingRoomfromMainStreet trans =
    case trans of
        (EnterReadingRoom_MainStreettoReadingRoom player msg)  -> (unwrapReadingRoomPlayer player, Just $ unwrapDidEnterReadingRoom msg)




unwrapEnterEditingRoomfromMainStreet :: EnterEditingRoomfromMainStreet -> (Player, Maybe ClientMessage)
unwrapEnterEditingRoomfromMainStreet trans =
    case trans of
        (EnterEditingRoom_MainStreettoReadingRoom player msg)  -> (unwrapReadingRoomPlayer player, Just $ unwrapDidEnterEditingRoom msg)




unwrapStartEditingfromEditingRoom :: StartEditingfromEditingRoom -> (Player, Maybe ClientMessage)
unwrapStartEditingfromEditingRoom trans =
    case trans of
        (StartEditing_EditingRoomtoEditingRoom player msg)  -> (unwrapEditingRoomPlayer player, fmap unwrapDidStartEditing msg)




unwrapLeaveReadingRoomfromReadingRoom :: LeaveReadingRoomfromReadingRoom -> (Player, Maybe ClientMessage)
unwrapLeaveReadingRoomfromReadingRoom trans =
    case trans of
        (LeaveReadingRoom_ReadingRoomtoMainStreet player msg)  -> (unwrapMainStreetPlayer player, Just $ unwrapDidLeaveReadingRoom msg)




unwrapLeaveEditingRoomfromEditingRoom :: LeaveEditingRoomfromEditingRoom -> (Player, Maybe ClientMessage)
unwrapLeaveEditingRoomfromEditingRoom trans =
    case trans of
        (LeaveEditingRoom_EditingRoomtoMainStreet player msg)  -> (unwrapMainStreetPlayer player, Just $ unwrapDidLeaveEditingRoom msg)




unwrapPublishArticlefromEditingRoom :: PublishArticlefromEditingRoom -> (Player, Maybe ClientMessage)
unwrapPublishArticlefromEditingRoom trans =
    case trans of
        (PublishArticle_EditingRoomtoEditingRoom player)  -> (unwrapEditingRoomPlayer player, Nothing)
        (PublishArticle_EditingRoomtoReadingRoom player msg)  -> (unwrapReadingRoomPlayer player, Just $ unwrapDidPublish msg)




unwrapSaveDraftfromEditingRoom :: SaveDraftfromEditingRoom -> (Player, Maybe ClientMessage)
unwrapSaveDraftfromEditingRoom trans =
    case trans of
        (SaveDraft_EditingRoomtoEditingRoom player msg)  -> (unwrapEditingRoomPlayer player, fmap unwrapDidSaveDraft msg)




unwrapEnterTitlefromEditingRoom :: EnterTitlefromEditingRoom -> (Player, Maybe ClientMessage)
unwrapEnterTitlefromEditingRoom trans =
    case trans of
        (EnterTitle_EditingRoomtoEditingRoom player msg)  -> (unwrapEditingRoomPlayer player, fmap unwrapDidEnterTitle msg)




unwrapEnterTextfromEditingRoom :: EnterTextfromEditingRoom -> (Player, Maybe ClientMessage)
unwrapEnterTextfromEditingRoom trans =
    case trans of
        (EnterText_EditingRoomtoEditingRoom player msg)  -> (unwrapEditingRoomPlayer player, fmap unwrapDidEnterText msg)




unwrapEnterCommentfromEditingRoom :: EnterCommentfromEditingRoom -> (Player, Maybe ClientMessage)
unwrapEnterCommentfromEditingRoom trans =
    case trans of
        (EnterComment_EditingRoomtoEditingRoom player msg)  -> (unwrapEditingRoomPlayer player, fmap unwrapDidEnterComment msg)




unwrapPostCommentfromEditingRoom :: PostCommentfromEditingRoom -> (Player, Maybe ClientMessage)
unwrapPostCommentfromEditingRoom trans =
    case trans of
        (PostComment_EditingRoomtoEditingRoom player msg)  -> (unwrapEditingRoomPlayer player, fmap unwrapDidPostComment msg)





unwrapEnterReadingRoom :: EnterReadingRoom -> Transition
unwrapEnterReadingRoom EnterReadingRoom  = TEnterReadingRoom 


unwrapEnterEditingRoom :: EnterEditingRoom -> Transition
unwrapEnterEditingRoom EnterEditingRoom  = TEnterEditingRoom 


unwrapStartEditing :: StartEditing -> Transition
unwrapStartEditing (StartEditing title)  = (TStartEditing title) 


unwrapLeaveReadingRoom :: LeaveReadingRoom -> Transition
unwrapLeaveReadingRoom LeaveReadingRoom  = TLeaveReadingRoom 


unwrapLeaveEditingRoom :: LeaveEditingRoom -> Transition
unwrapLeaveEditingRoom LeaveEditingRoom  = TLeaveEditingRoom 


unwrapPublishArticle :: PublishArticle -> Transition
unwrapPublishArticle PublishArticle  = TPublishArticle 


unwrapSaveDraft :: SaveDraft -> Transition
unwrapSaveDraft (SaveDraft draft)  = (TSaveDraft draft) 


unwrapEnterTitle :: EnterTitle -> Transition
unwrapEnterTitle (EnterTitle title)  = (TEnterTitle title) 


unwrapEnterText :: EnterText -> Transition
unwrapEnterText (EnterText text)  = (TEnterText text) 


unwrapEnterComment :: EnterComment -> Transition
unwrapEnterComment (EnterComment comment)  = (TEnterComment comment) 


unwrapPostComment :: PostComment -> Transition
unwrapPostComment (PostComment comment)  = (TPostComment comment) 



wrapEnterReadingRoom :: Transition -> EnterReadingRoom
wrapEnterReadingRoom x__ =
    case x__ of
        TEnterReadingRoom  -> EnterReadingRoom 
        _ -> EnterReadingRoom 


wrapEnterEditingRoom :: Transition -> EnterEditingRoom
wrapEnterEditingRoom x__ =
    case x__ of
        TEnterEditingRoom  -> EnterEditingRoom 
        _ -> EnterEditingRoom 


wrapStartEditing :: Transition -> StartEditing
wrapStartEditing x__ =
    case x__ of
        (TStartEditing title)  -> (StartEditing title) 
        _ -> StartEditing ""


wrapLeaveReadingRoom :: Transition -> LeaveReadingRoom
wrapLeaveReadingRoom x__ =
    case x__ of
        TLeaveReadingRoom  -> LeaveReadingRoom 
        _ -> LeaveReadingRoom 


wrapLeaveEditingRoom :: Transition -> LeaveEditingRoom
wrapLeaveEditingRoom x__ =
    case x__ of
        TLeaveEditingRoom  -> LeaveEditingRoom 
        _ -> LeaveEditingRoom 


wrapPublishArticle :: Transition -> PublishArticle
wrapPublishArticle x__ =
    case x__ of
        TPublishArticle  -> PublishArticle 
        _ -> PublishArticle 


wrapSaveDraft :: Transition -> SaveDraft
wrapSaveDraft x__ =
    case x__ of
        (TSaveDraft draft)  -> (SaveDraft draft) 
        _ -> SaveDraft (DraftArticle "" "" 0 "" [])


wrapEnterTitle :: Transition -> EnterTitle
wrapEnterTitle x__ =
    case x__ of
        (TEnterTitle title)  -> (EnterTitle title) 
        _ -> EnterTitle ""


wrapEnterText :: Transition -> EnterText
wrapEnterText x__ =
    case x__ of
        (TEnterText text)  -> (EnterText text) 
        _ -> EnterText ""


wrapEnterComment :: Transition -> EnterComment
wrapEnterComment x__ =
    case x__ of
        (TEnterComment comment)  -> (EnterComment comment) 
        _ -> EnterComment ""


wrapPostComment :: Transition -> PostComment
wrapPostComment x__ =
    case x__ of
        (TPostComment comment)  -> (PostComment comment) 
        _ -> PostComment ""



