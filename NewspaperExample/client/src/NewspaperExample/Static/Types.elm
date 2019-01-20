module NewspaperExample.Static.Types exposing(..)

-- the types of all places in the net
-- place states
type MainStreet  =
      MainStreet


type ReadingRoom  =
      ReadingRoom (List Article {-article-}) {-articles-} (List String {-title-}) {-titles-} (Maybe String {-viewing-}) {-maybeViewing-}


type EditingRoom  =
      EditingRoom (Maybe Draft {-article-}) {-maybeEditing-} (List String {-title-}) {-titles-}



-- union place type
type NetState  =
      SMainStreet MainStreet
    | SReadingRoom ReadingRoom
    | SEditingRoom EditingRoom
-- server transition types
type OutgoingTransition  =
      TEnterReadingRoom
    | TEnterEditingRoom
    | TStartEditing String
    | TLeaveReadingRoom
    | TLeaveEditingRoom
    | TPublishArticle
    | TSaveDraft Draft
    | TEnterTitle String
    | TEnterText String
    | TEnterComment String
    | TPostComment String
type EnterReadingRoom  =
      EnterReadingRoom
type EnterEditingRoom  =
      EnterEditingRoom
type StartEditing  =
      StartEditing String
type LeaveReadingRoom  =
      LeaveReadingRoom
type LeaveEditingRoom  =
      LeaveEditingRoom
type PublishArticle  =
      PublishArticle
type SaveDraft  =
      SaveDraft Draft
type EnterTitle  =
      EnterTitle String
type EnterText  =
      EnterText String
type EnterComment  =
      EnterComment String
type PostComment  =
      PostComment String

-- outgoing server message types
type DidEnterReadingRoom  =
      DidEnterReadingRoom (List Article {-article-}) {-articles-}
type DidEnterEditingRoom  =
      DidEnterEditingRoom (List String {-title-}) {-articles-}
type DidStartEditing  =
      DidStartEditing Draft {-draft-}
type DidLeaveReadingRoom  =
      DidLeaveReadingRoom
type DidLeaveEditingRoom  =
      DidLeaveEditingRoom
type DidPublish  =
      DidPublish (List Article {-article-}) {-articles-}
type DidSaveDraft  =
      DidSaveDraft (List String {-article-}) {-articles-}
type DidEnterTitle  =
      DidEnterTitle (List String {-article-}) {-articles-}
type DidEnterText  =
      DidEnterText (List String {-article-}) {-articles-}
type DidEnterComment  =
      DidEnterComment String {-comment-}
type DidPostComment  =
      DidPostComment String {-comment-}
type IncomingMessage  =
      MDidEnterReadingRoom (List Article {-article-}) {-articles-}
    | MDidEnterEditingRoom (List String {-title-}) {-articles-}
    | MDidStartEditing Draft {-draft-}
    | MDidLeaveReadingRoom
    | MDidLeaveEditingRoom
    | MDidPublish (List Article {-article-}) {-articles-}
    | MDidSaveDraft (List String {-article-}) {-articles-}
    | MDidEnterTitle (List String {-article-}) {-articles-}
    | MDidEnterText (List String {-article-}) {-articles-}
    | MDidEnterComment String {-comment-}
    | MDidPostComment String {-comment-}

-- extra server types
type Article  =
      Article String String Int String
    | Letter String String Int String
type Draft  =
      DraftArticle String String Int String (List (Int, String))
    | DraftLetter String String Int String (List (Int, String))


