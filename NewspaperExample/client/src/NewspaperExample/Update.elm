module NewspaperExample.Update exposing(..)
import NewspaperExample.Static.Types exposing(..)
import NewspaperExample.Static.FromSuperPlace exposing(..)
import Utils.Utils
import Debug exposing(todo)

updateMainStreetDidEnterReadingRoomReadingRoom : FromSuperPlace -> DidEnterReadingRoom -> MainStreet -> ReadingRoom
updateMainStreetDidEnterReadingRoomReadingRoom fsp (DidEnterReadingRoom articles)  mainStreet =
  let
      getTitles art = case art of
        Article title _ _ _ -> title
        Letter title _ _ _ -> title
  in
      ReadingRoom articles {-articles-} (List.map getTitles articles) {-titles-} (Nothing) {-maybeViewing-}


updateMainStreetDidEnterEditingRoomReadingRoom : FromSuperPlace -> DidEnterEditingRoom -> MainStreet -> ReadingRoom
updateMainStreetDidEnterEditingRoomReadingRoom fsp (DidEnterEditingRoom articles)  mainStreet =
  let
      getTitles art = case art of
        DraftArticle title _ _ _ -> title
        DraftLetter title _ _ _ -> title
  in
      EditingRoom Nothing {-maybeEditing-} (List String {-title-}) {-titles-}


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


updateEditingRoomDidEnterCommentEditingRoom : FromSuperPlace -> DidEnterComment -> EditingRoom -> EditingRoom
updateEditingRoomDidEnterCommentEditingRoom fsp (DidEnterComment comment)  editingRoom =
    todo "Please implement update function updateEditingRoomDidEnterCommentEditingRoom for the NewspaperExample net."
