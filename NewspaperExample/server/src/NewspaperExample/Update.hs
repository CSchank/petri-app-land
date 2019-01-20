module NewspaperExample.Update where
import NewspaperExample.Static.Types
import NewspaperExample.Static.FromSuperPlace
import Static.List
import Utils.Utils
import Static.ServerTypes

-- function called when new client connects (do not delete)
clientConnect :: FromSuperPlace -> ClientID -> MainStreet -> (MainStreet, MainStreetPlayer)
clientConnect fsp clientID mainStreet =
    error "Please fill out clientConnect function for the NewspaperExample net."

-- functions called when a client disconnects (do not delete)
clientDisconnectFromEditingRoom :: FromSuperPlace -> ClientID -> EditingRoom -> EditingRoomPlayer -> EditingRoom
clientDisconnectFromEditingRoom fsp clientID editingRoom editingRoomPlayer =
    error "Please fill out the clientDisconnectFromEditingRoom function for the NewspaperExample net."

clientDisconnectFromMainStreet :: FromSuperPlace -> ClientID -> MainStreet -> MainStreetPlayer -> MainStreet
clientDisconnectFromMainStreet fsp clientID mainStreet mainStreetPlayer =
    error "Please fill out the clientDisconnectFromMainStreet function for the NewspaperExample net."

clientDisconnectFromReadingRoom :: FromSuperPlace -> ClientID -> ReadingRoom -> ReadingRoomPlayer -> ReadingRoom
clientDisconnectFromReadingRoom fsp clientID readingRoom readingRoomPlayer =
    error "Please fill out the clientDisconnectFromReadingRoom function for the NewspaperExample net."


-- functions for each transition
updateEnterReadingRoom :: FromSuperPlace -> Maybe ClientID -> EnterReadingRoom -> MainStreet -> ReadingRoom -> List MainStreetPlayer -> (MainStreet, ReadingRoom, MainStreetPlayer -> EnterReadingRoomfromMainStreet)
updateEnterReadingRoom fsp mClientId EnterReadingRoom mainStreet readingRoom lstMainStreet =
    let
        fromMainStreet :: MainStreetPlayer -> EnterReadingRoomfromMainStreet
        fromMainStreet pmainStreet = error "Please fill in function stub."


    in
        (mainStreet, readingRoom, fromMainStreet)

updateEnterEditingRoom :: FromSuperPlace -> Maybe ClientID -> EnterEditingRoom -> MainStreet -> ReadingRoom -> List MainStreetPlayer -> (MainStreet, ReadingRoom, MainStreetPlayer -> EnterEditingRoomfromMainStreet)
updateEnterEditingRoom fsp mClientId EnterEditingRoom mainStreet readingRoom lstMainStreet =
    let
        fromMainStreet :: MainStreetPlayer -> EnterEditingRoomfromMainStreet
        fromMainStreet pmainStreet = error "Please fill in function stub."


    in
        (mainStreet, readingRoom, fromMainStreet)

updateStartEditing :: FromSuperPlace -> Maybe ClientID -> StartEditing -> EditingRoom -> List EditingRoomPlayer -> (EditingRoom, EditingRoomPlayer -> StartEditingfromEditingRoom)
updateStartEditing fsp mClientId (StartEditing title) editingRoom lstEditingRoom =
    let
        fromEditingRoom :: EditingRoomPlayer -> StartEditingfromEditingRoom
        fromEditingRoom peditingRoom = error "Please fill in function stub."


    in
        (editingRoom, fromEditingRoom)

updateLeaveReadingRoom :: FromSuperPlace -> Maybe ClientID -> LeaveReadingRoom -> ReadingRoom -> MainStreet -> List ReadingRoomPlayer -> (ReadingRoom, MainStreet, ReadingRoomPlayer -> LeaveReadingRoomfromReadingRoom)
updateLeaveReadingRoom fsp mClientId LeaveReadingRoom readingRoom mainStreet lstReadingRoom =
    let
        fromReadingRoom :: ReadingRoomPlayer -> LeaveReadingRoomfromReadingRoom
        fromReadingRoom preadingRoom = error "Please fill in function stub."


    in
        (readingRoom, mainStreet, fromReadingRoom)

updateLeaveEditingRoom :: FromSuperPlace -> Maybe ClientID -> LeaveEditingRoom -> EditingRoom -> MainStreet -> List EditingRoomPlayer -> (EditingRoom, MainStreet, EditingRoomPlayer -> LeaveEditingRoomfromEditingRoom)
updateLeaveEditingRoom fsp mClientId LeaveEditingRoom editingRoom mainStreet lstEditingRoom =
    let
        fromEditingRoom :: EditingRoomPlayer -> LeaveEditingRoomfromEditingRoom
        fromEditingRoom peditingRoom = error "Please fill in function stub."


    in
        (editingRoom, mainStreet, fromEditingRoom)

updatePublishArticle :: FromSuperPlace -> Maybe ClientID -> PublishArticle -> EditingRoom -> ReadingRoom -> List EditingRoomPlayer -> (EditingRoom, ReadingRoom, EditingRoomPlayer -> PublishArticlefromEditingRoom)
updatePublishArticle fsp mClientId PublishArticle editingRoom readingRoom lstEditingRoom =
    let
        fromEditingRoom :: EditingRoomPlayer -> PublishArticlefromEditingRoom
        fromEditingRoom peditingRoom = error "Please fill in function stub."


    in
        (editingRoom, readingRoom, fromEditingRoom)

updateSaveDraft :: FromSuperPlace -> Maybe ClientID -> SaveDraft -> EditingRoom -> List EditingRoomPlayer -> (EditingRoom, EditingRoomPlayer -> SaveDraftfromEditingRoom)
updateSaveDraft fsp mClientId (SaveDraft draft) editingRoom lstEditingRoom =
    let
        fromEditingRoom :: EditingRoomPlayer -> SaveDraftfromEditingRoom
        fromEditingRoom peditingRoom = error "Please fill in function stub."


    in
        (editingRoom, fromEditingRoom)

updateEnterTitle :: FromSuperPlace -> Maybe ClientID -> EnterTitle -> EditingRoom -> List EditingRoomPlayer -> (EditingRoom, EditingRoomPlayer -> EnterTitlefromEditingRoom)
updateEnterTitle fsp mClientId (EnterTitle title) editingRoom lstEditingRoom =
    let
        fromEditingRoom :: EditingRoomPlayer -> EnterTitlefromEditingRoom
        fromEditingRoom peditingRoom = error "Please fill in function stub."


    in
        (editingRoom, fromEditingRoom)

updateEnterText :: FromSuperPlace -> Maybe ClientID -> EnterText -> EditingRoom -> List EditingRoomPlayer -> (EditingRoom, EditingRoomPlayer -> EnterTextfromEditingRoom)
updateEnterText fsp mClientId (EnterText text) editingRoom lstEditingRoom =
    let
        fromEditingRoom :: EditingRoomPlayer -> EnterTextfromEditingRoom
        fromEditingRoom peditingRoom = error "Please fill in function stub."


    in
        (editingRoom, fromEditingRoom)

updateEnterComment :: FromSuperPlace -> Maybe ClientID -> EnterComment -> EditingRoom -> List EditingRoomPlayer -> (EditingRoom, EditingRoomPlayer -> EnterCommentfromEditingRoom)
updateEnterComment fsp mClientId (EnterComment comment) editingRoom lstEditingRoom =
    let
        fromEditingRoom :: EditingRoomPlayer -> EnterCommentfromEditingRoom
        fromEditingRoom peditingRoom = error "Please fill in function stub."


    in
        (editingRoom, fromEditingRoom)

updatePostComment :: FromSuperPlace -> Maybe ClientID -> PostComment -> EditingRoom -> List EditingRoomPlayer -> (EditingRoom, EditingRoomPlayer -> PostCommentfromEditingRoom)
updatePostComment fsp mClientId (PostComment comment) editingRoom lstEditingRoom =
    let
        fromEditingRoom :: EditingRoomPlayer -> PostCommentfromEditingRoom
        fromEditingRoom peditingRoom = error "Please fill in function stub."


    in
        (editingRoom, fromEditingRoom)


