module NewspaperExample.Static.Update exposing(..)
import NewspaperExample.Static.Types exposing(..)
import NewspaperExample.Static.Wrappers exposing(..)
import NewspaperExample.Static.FromSuperPlace exposing (FromSuperPlace)
import NewspaperExample.Update exposing(..)

update : FromSuperPlace -> IncomingMessage -> NetState -> (NetState,Maybe (Cmd OutgoingTransition))
update fsp trans state =
    case (trans,state) of
        ((MDidEnterReadingRoom _) , SMainStreet st) -> (SReadingRoom <| updateMainStreetDidEnterReadingRoomReadingRoom fsp (wrapDidEnterReadingRoom trans) st, Nothing)

        ((MDidEnterEditingRoom _) , SMainStreet st) -> (SReadingRoom <| updateMainStreetDidEnterEditingRoomReadingRoom fsp (wrapDidEnterEditingRoom trans) st, Nothing)

        ((MDidStartEditing _) , SEditingRoom st) -> (SEditingRoom <| updateEditingRoomDidStartEditingEditingRoom fsp (wrapDidStartEditing trans) st, Nothing)

        (MDidLeaveReadingRoom , SReadingRoom st) -> (SMainStreet <| updateReadingRoomDidLeaveReadingRoomMainStreet fsp (wrapDidLeaveReadingRoom trans) st, Nothing)

        (MDidLeaveEditingRoom , SEditingRoom st) -> (SMainStreet <| updateEditingRoomDidLeaveEditingRoomMainStreet fsp (wrapDidLeaveEditingRoom trans) st, Nothing)

        ((MDidPublish _) , SEditingRoom st) -> (SReadingRoom <| updateEditingRoomDidPublishReadingRoom fsp (wrapDidPublish trans) st, Nothing)

        ((MDidSaveDraft _) , SEditingRoom st) -> (SEditingRoom <| updateEditingRoomDidSaveDraftEditingRoom fsp (wrapDidSaveDraft trans) st, Nothing)

        ((MDidEnterTitle _) , SEditingRoom st) -> (SEditingRoom <| updateEditingRoomDidEnterTitleEditingRoom fsp (wrapDidEnterTitle trans) st, Nothing)

        ((MDidEnterText _) , SEditingRoom st) -> (SEditingRoom <| updateEditingRoomDidEnterTextEditingRoom fsp (wrapDidEnterText trans) st, Nothing)

        ((MDidEnterComment _) , SEditingRoom st) -> (SEditingRoom <| updateEditingRoomDidEnterCommentEditingRoom fsp (wrapDidEnterComment trans) st, Nothing)

        ((MDidPostComment _) , SEditingRoom st) -> (SEditingRoom <| updateEditingRoomDidPostCommentEditingRoom fsp (wrapDidPostComment trans) st, Nothing)


        _ -> (state, Nothing)
