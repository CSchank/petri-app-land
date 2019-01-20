module NewspaperExample.Static.Types.EditingRoom exposing(..)
import NewspaperExample.Static.Types exposing(..)

type Msg  =
      StartEditing String
    | LeaveEditingRoom
    | PublishArticle
    | SaveDraft Draft
    | EnterTitle String
    | EnterText String
    | EnterComment String
    | PostComment String
