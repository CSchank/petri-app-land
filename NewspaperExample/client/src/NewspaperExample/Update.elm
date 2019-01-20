module NewspaperExample.Update exposing(..)
import NewspaperExample.Static.Types exposing(..)
import NewspaperExample.Static.FromSuperPlace exposing(..)
import Utils.Utils
import Debug exposing(todo)

updateMainStreetDidEnterReadingRoomReadingRoom : FromSuperPlace -> DidEnterReadingRoom -> MainStreet -> ReadingRoom
updateMainStreetDidEnterReadingRoomReadingRoom fsp (DidEnterReadingRoom articles)  mainStreet =
    todo "Please implement update function updateMainStreetDidEnterReadingRoomReadingRoom for the NewspaperExample net."


updateMainStreetDidEnterEditingRoomReadingRoom : FromSuperPlace -> DidEnterEditingRoom -> MainStreet -> ReadingRoom
updateMainStreetDidEnterEditingRoomReadingRoom fsp (DidEnterEditingRoom articles)  mainStreet =
    todo "Please implement update function updateMainStreetDidEnterEditingRoomReadingRoom for the NewspaperExample net."


updateEditingRoomDidStartEditingEditingRoom : FromSuperPlace -> DidStartEditing -> EditingRoom -> EditingRoom
updateEditingRoomDidStartEditingEditingRoom fsp (DidStartEditing draft)  editingRoom =
    todo "Please implement update function updateEditingRoomDidStartEditingEditingRoom for the NewspaperExample net."


updateReadingRoomDidLeaveReadingRoomMainStreet : FromSuperPlace -> DidLeaveReadingRoom -> ReadingRoom -> MainStreet
updateReadingRoomDidLeaveReadingRoomMainStreet fsp DidLeaveReadingRoom  readingRoom =
    todo "Please implement update function updateReadingRoomDidLeaveReadingRoomMainStreet for the NewspaperExample net."


updateEditingRoomDidLeaveEditingRoomMainStreet : FromSuperPlace -> DidLeaveEditingRoom -> EditingRoom -> MainStreet
updateEditingRoomDidLeaveEditingRoomMainStreet fsp DidLeaveEditingRoom  editingRoom =
    todo "Please implement update function updateEditingRoomDidLeaveEditingRoomMainStreet for the NewspaperExample net."


updateEditingRoomDidPublishReadingRoom : FromSuperPlace -> DidPublish -> EditingRoom -> ReadingRoom
updateEditingRoomDidPublishReadingRoom fsp (DidPublish articles)  editingRoom =
    todo "Please implement update function updateEditingRoomDidPublishReadingRoom for the NewspaperExample net."


updateEditingRoomDidSaveDraftEditingRoom : FromSuperPlace -> DidSaveDraft -> EditingRoom -> EditingRoom
updateEditingRoomDidSaveDraftEditingRoom fsp (DidSaveDraft articles)  editingRoom =
    todo "Please implement update function updateEditingRoomDidSaveDraftEditingRoom for the NewspaperExample net."


updateEditingRoomDidEnterTitleEditingRoom : FromSuperPlace -> DidEnterTitle -> EditingRoom -> EditingRoom
updateEditingRoomDidEnterTitleEditingRoom fsp (DidEnterTitle articles)  editingRoom =
    todo "Please implement update function updateEditingRoomDidEnterTitleEditingRoom for the NewspaperExample net."


updateEditingRoomDidEnterTextEditingRoom : FromSuperPlace -> DidEnterText -> EditingRoom -> EditingRoom
updateEditingRoomDidEnterTextEditingRoom fsp (DidEnterText articles)  editingRoom =
    todo "Please implement update function updateEditingRoomDidEnterTextEditingRoom for the NewspaperExample net."


updateEditingRoomDidEnterCommentEditingRoom : FromSuperPlace -> DidEnterComment -> EditingRoom -> EditingRoom
updateEditingRoomDidEnterCommentEditingRoom fsp (DidEnterComment comment)  editingRoom =
    todo "Please implement update function updateEditingRoomDidEnterCommentEditingRoom for the NewspaperExample net."


updateEditingRoomDidPostCommentEditingRoom : FromSuperPlace -> DidPostComment -> EditingRoom -> EditingRoom
updateEditingRoomDidPostCommentEditingRoom fsp (DidPostComment comment)  editingRoom =
    todo "Please implement update function updateEditingRoomDidPostCommentEditingRoom for the NewspaperExample net."



