module NewspaperExample.Static.Types where
import Data.Typeable (Typeable)
import Static.List

-- the initial state of all places in this net
-- place states and place player states
data MainStreet  =
      MainStreet
    deriving(Ord,Eq,Show,Typeable)

data MainStreetPlayer  =
      MainStreetPlayer
    deriving(Ord,Eq,Show,Typeable)


data ReadingRoom  =
      ReadingRoom (List Article {-article-}) {-articles-}
    deriving(Ord,Eq,Show,Typeable)

data ReadingRoomPlayer  =
      ReadingRoomPlayer String {-nowReading-}
    deriving(Ord,Eq,Show,Typeable)


data EditingRoom  =
      EditingRoom (List Draft {-drafts-}) {-articles-}
    deriving(Ord,Eq,Show,Typeable)

data EditingRoomPlayer  =
      EditingRoomPlayer (Maybe String {-nowEditing-}) {-maybeEditing-}
    deriving(Ord,Eq,Show,Typeable)



-- outgoing client message types
data DidEnterReadingRoom  =
      DidEnterReadingRoom (List Article {-article-}) {-articles-}
    deriving(Ord,Eq,Show)
data DidEnterEditingRoom  =
      DidEnterEditingRoom (List String {-title-}) {-articles-}
    deriving(Ord,Eq,Show)
data DidStartEditing  =
      DidStartEditing Draft {-draft-}
    deriving(Ord,Eq,Show)
data DidLeaveReadingRoom  =
      DidLeaveReadingRoom
    deriving(Ord,Eq,Show)
data DidLeaveEditingRoom  =
      DidLeaveEditingRoom
    deriving(Ord,Eq,Show)
data DidPublish  =
      DidPublish (List Article {-article-}) {-articles-}
    deriving(Ord,Eq,Show)
data DidSaveDraft  =
      DidSaveDraft (List String {-article-}) {-articles-}
    deriving(Ord,Eq,Show)
data DidEnterTitle  =
      DidEnterTitle (List String {-article-}) {-articles-}
    deriving(Ord,Eq,Show)
data DidEnterText  =
      DidEnterText (List String {-article-}) {-articles-}
    deriving(Ord,Eq,Show)
data DidEnterComment  =
      DidEnterComment String {-comment-}
    deriving(Ord,Eq,Show)
data DidPostComment  =
      DidPostComment String {-comment-}
    deriving(Ord,Eq,Show)
data ClientMessage  =
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
    deriving(Ord,Eq,Show)

-- individual transition types
data EnterReadingRoomfromMainStreet  =
      EnterReadingRoom_MainStreettoReadingRoom ReadingRoomPlayer DidEnterReadingRoom
    deriving(Ord,Eq,Show)

data EnterEditingRoomfromMainStreet  =
      EnterEditingRoom_MainStreettoReadingRoom ReadingRoomPlayer DidEnterEditingRoom
    deriving(Ord,Eq,Show)

data StartEditingfromEditingRoom  =
      StartEditing_EditingRoomtoEditingRoom EditingRoomPlayer DidStartEditing
    deriving(Ord,Eq,Show)

data LeaveReadingRoomfromReadingRoom  =
      LeaveReadingRoom_ReadingRoomtoMainStreet MainStreetPlayer DidLeaveReadingRoom
    deriving(Ord,Eq,Show)

data LeaveEditingRoomfromEditingRoom  =
      LeaveEditingRoom_EditingRoomtoMainStreet MainStreetPlayer DidLeaveEditingRoom
    deriving(Ord,Eq,Show)

data PublishArticlefromEditingRoom  =
      PublishArticle_EditingRoomtoEditingRoom EditingRoomPlayer
    | PublishArticle_EditingRoomtoReadingRoom ReadingRoomPlayer DidPublish
    deriving(Ord,Eq,Show)

data SaveDraftfromEditingRoom  =
      SaveDraft_EditingRoomtoEditingRoom EditingRoomPlayer DidSaveDraft
    deriving(Ord,Eq,Show)

data EnterTitlefromEditingRoom  =
      EnterTitle_EditingRoomtoEditingRoom EditingRoomPlayer DidEnterTitle
    deriving(Ord,Eq,Show)

data EnterTextfromEditingRoom  =
      EnterText_EditingRoomtoEditingRoom EditingRoomPlayer DidEnterText
    deriving(Ord,Eq,Show)

data EnterCommentfromEditingRoom  =
      EnterComment_EditingRoomtoEditingRoom EditingRoomPlayer DidEnterComment
    deriving(Ord,Eq,Show)

data PostCommentfromEditingRoom  =
      PostComment_EditingRoomtoEditingRoom EditingRoomPlayer DidPostComment
    deriving(Ord,Eq,Show)


-- main transition types
data Transition  =
      TEnterReadingRoom
    | TEnterEditingRoom
    | TStartEditing String {-title-}
    | TLeaveReadingRoom
    | TLeaveEditingRoom
    | TPublishArticle
    | TSaveDraft Draft {-draft-}
    | TEnterTitle String {-title-}
    | TEnterText String {-text-}
    | TEnterComment String {-comment-}
    | TPostComment String {-comment-}
    deriving(Ord,Eq,Show)
data EnterReadingRoom  =
      EnterReadingRoom
    deriving(Ord,Eq,Show)
data EnterEditingRoom  =
      EnterEditingRoom
    deriving(Ord,Eq,Show)
data StartEditing  =
      StartEditing String {-title-}
    deriving(Ord,Eq,Show)
data LeaveReadingRoom  =
      LeaveReadingRoom
    deriving(Ord,Eq,Show)
data LeaveEditingRoom  =
      LeaveEditingRoom
    deriving(Ord,Eq,Show)
data PublishArticle  =
      PublishArticle
    deriving(Ord,Eq,Show)
data SaveDraft  =
      SaveDraft Draft {-draft-}
    deriving(Ord,Eq,Show)
data EnterTitle  =
      EnterTitle String {-title-}
    deriving(Ord,Eq,Show)
data EnterText  =
      EnterText String {-text-}
    deriving(Ord,Eq,Show)
data EnterComment  =
      EnterComment String {-comment-}
    deriving(Ord,Eq,Show)
data PostComment  =
      PostComment String {-comment-}
    deriving(Ord,Eq,Show)

-- player state union type
data Player  =
      PMainStreetPlayer
    | PReadingRoomPlayer String
    | PEditingRoomPlayer (Maybe String)
    deriving(Ord,Eq,Show)
-- extra server types
data Article  =
      Article String String Int String
    | Letter String String Int String
    deriving(Ord,Eq,Show)
data Draft  =
      DraftArticle String String Int String (List (Int, String))
    | DraftLetter String String Int String (List (Int, String))
    deriving(Ord,Eq,Show)


-- the FromSuperPlace type
