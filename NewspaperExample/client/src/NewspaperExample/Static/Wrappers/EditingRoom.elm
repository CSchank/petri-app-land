module NewspaperExample.Static.Wrappers.EditingRoom exposing(..)
import NewspaperExample.Static.Types.EditingRoom exposing(..)
import NewspaperExample.Static.Types exposing(..)

unwrap : Msg -> OutgoingTransition
unwrap msg =
    case msg of
        (NewspaperExample.Static.Types.EditingRoom.StartEditing title)  -> TEnterReadingRoom 
        NewspaperExample.Static.Types.EditingRoom.LeaveEditingRoom  -> TEnterEditingRoom 
        NewspaperExample.Static.Types.EditingRoom.PublishArticle  -> (TStartEditing title) 
        (NewspaperExample.Static.Types.EditingRoom.SaveDraft draft)  -> TLeaveReadingRoom 
        (NewspaperExample.Static.Types.EditingRoom.EnterTitle title)  -> TLeaveEditingRoom 
        (NewspaperExample.Static.Types.EditingRoom.EnterText text)  -> TPublishArticle 
        (NewspaperExample.Static.Types.EditingRoom.EnterComment comment)  -> (TSaveDraft draft) 
        (NewspaperExample.Static.Types.EditingRoom.PostComment comment)  -> (TEnterTitle title) 

