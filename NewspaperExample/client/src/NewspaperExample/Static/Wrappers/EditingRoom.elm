module NewspaperExample.Static.Wrappers.EditingRoom exposing(..)
import NewspaperExample.Static.Types.EditingRoom exposing(..)
import NewspaperExample.Static.Types exposing(..)

unwrap : Msg -> OutgoingTransition
unwrap msg =
    case msg of
        (NewspaperExample.Static.Types.EditingRoom.StartEditing title)  -> (TStartEditing title) 
        NewspaperExample.Static.Types.EditingRoom.LeaveEditingRoom  -> TLeaveEditingRoom 
        NewspaperExample.Static.Types.EditingRoom.PublishArticle  -> TPublishArticle 
        (NewspaperExample.Static.Types.EditingRoom.SaveDraft draft)  -> (TSaveDraft draft) 
        (NewspaperExample.Static.Types.EditingRoom.EnterTitle title)  -> (TEnterTitle title) 
        (NewspaperExample.Static.Types.EditingRoom.EnterText text)  -> (TEnterText text) 
        (NewspaperExample.Static.Types.EditingRoom.EnterComment comment)  -> (TEnterComment comment) 
        (NewspaperExample.Static.Types.EditingRoom.PostComment comment)  -> (TPostComment comment) 

