module NewspaperExample.Static.Wrappers exposing(..)
import NewspaperExample.Static.Types exposing(..)

wrapDidEnterReadingRoom : IncomingMessage -> DidEnterReadingRoom
wrapDidEnterReadingRoom x__ =
    case x__ of
        (MDidEnterReadingRoom articles)  -> (DidEnterReadingRoom articles) 
        _ -> DidEnterReadingRoom []


wrapDidEnterEditingRoom : IncomingMessage -> DidEnterEditingRoom
wrapDidEnterEditingRoom x__ =
    case x__ of
        (MDidEnterEditingRoom articles)  -> (DidEnterEditingRoom articles) 
        _ -> DidEnterEditingRoom []


wrapDidStartEditing : IncomingMessage -> DidStartEditing
wrapDidStartEditing x__ =
    case x__ of
        (MDidStartEditing draft)  -> (DidStartEditing draft) 
        _ -> DidStartEditing (DraftArticle "" "" 0 "" [])


wrapDidLeaveReadingRoom : IncomingMessage -> DidLeaveReadingRoom
wrapDidLeaveReadingRoom x__ =
    case x__ of
        MDidLeaveReadingRoom  -> DidLeaveReadingRoom 
        _ -> DidLeaveReadingRoom 


wrapDidLeaveEditingRoom : IncomingMessage -> DidLeaveEditingRoom
wrapDidLeaveEditingRoom x__ =
    case x__ of
        MDidLeaveEditingRoom  -> DidLeaveEditingRoom 
        _ -> DidLeaveEditingRoom 


wrapDidPublish : IncomingMessage -> DidPublish
wrapDidPublish x__ =
    case x__ of
        (MDidPublish articles)  -> (DidPublish articles) 
        _ -> DidPublish []


wrapDidSaveDraft : IncomingMessage -> DidSaveDraft
wrapDidSaveDraft x__ =
    case x__ of
        (MDidSaveDraft articles)  -> (DidSaveDraft articles) 
        _ -> DidSaveDraft []


wrapDidEnterTitle : IncomingMessage -> DidEnterTitle
wrapDidEnterTitle x__ =
    case x__ of
        (MDidEnterTitle articles)  -> (DidEnterTitle articles) 
        _ -> DidEnterTitle []


wrapDidEnterText : IncomingMessage -> DidEnterText
wrapDidEnterText x__ =
    case x__ of
        (MDidEnterText articles)  -> (DidEnterText articles) 
        _ -> DidEnterText []


wrapDidEnterComment : IncomingMessage -> DidEnterComment
wrapDidEnterComment x__ =
    case x__ of
        (MDidEnterComment comment)  -> (DidEnterComment comment) 
        _ -> DidEnterComment ""


wrapDidPostComment : IncomingMessage -> DidPostComment
wrapDidPostComment x__ =
    case x__ of
        (MDidPostComment comment)  -> (DidPostComment comment) 
        _ -> DidPostComment ""



unwrapEnterReadingRoom : EnterReadingRoom -> OutgoingTransition
unwrapEnterReadingRoom EnterReadingRoom  = TEnterReadingRoom 


unwrapEnterEditingRoom : EnterEditingRoom -> OutgoingTransition
unwrapEnterEditingRoom EnterEditingRoom  = TEnterEditingRoom 


unwrapStartEditing : StartEditing -> OutgoingTransition
unwrapStartEditing (StartEditing title)  = (TStartEditing title) 


unwrapLeaveReadingRoom : LeaveReadingRoom -> OutgoingTransition
unwrapLeaveReadingRoom LeaveReadingRoom  = TLeaveReadingRoom 


unwrapLeaveEditingRoom : LeaveEditingRoom -> OutgoingTransition
unwrapLeaveEditingRoom LeaveEditingRoom  = TLeaveEditingRoom 


unwrapPublishArticle : PublishArticle -> OutgoingTransition
unwrapPublishArticle PublishArticle  = TPublishArticle 


unwrapSaveDraft : SaveDraft -> OutgoingTransition
unwrapSaveDraft (SaveDraft draft)  = (TSaveDraft draft) 


unwrapEnterTitle : EnterTitle -> OutgoingTransition
unwrapEnterTitle (EnterTitle title)  = (TEnterTitle title) 


unwrapEnterText : EnterText -> OutgoingTransition
unwrapEnterText (EnterText text)  = (TEnterText text) 


unwrapEnterComment : EnterComment -> OutgoingTransition
unwrapEnterComment (EnterComment comment)  = (TEnterComment comment) 


unwrapPostComment : PostComment -> OutgoingTransition
unwrapPostComment (PostComment comment)  = (TPostComment comment) 



