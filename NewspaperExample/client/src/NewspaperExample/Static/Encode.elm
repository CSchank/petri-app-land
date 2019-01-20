module NewspaperExample.Static.Encode exposing(..)
import NewspaperExample.Static.Types exposing(..)

import Utils.Utils exposing(..)
import Static.Types
encodeOutgoingTransition : OutgoingTransition -> String
encodeOutgoingTransition outgoingtransition = 
    case outgoingtransition of
        TEnterReadingRoom ->                 tConcat ["TEnterReadingRoom\u{0000}"]
        TEnterEditingRoom ->                 tConcat ["TEnterEditingRoom\u{0000}"]
        TStartEditing title -> 
            let
                titleTxt = title
            in
                tConcat ["TStartEditing\u{0000}", titleTxt]
        TLeaveReadingRoom ->                 tConcat ["TLeaveReadingRoom\u{0000}"]
        TLeaveEditingRoom ->                 tConcat ["TLeaveEditingRoom\u{0000}"]
        TPublishArticle ->                 tConcat ["TPublishArticle\u{0000}"]
        TSaveDraft draft -> 
            let
                draftTxt = encodeDraft draft
            in
                tConcat ["TSaveDraft\u{0000}", draftTxt]
        TEnterTitle title -> 
            let
                titleTxt = title
            in
                tConcat ["TEnterTitle\u{0000}", titleTxt]
        TEnterText text -> 
            let
                textTxt = text
            in
                tConcat ["TEnterText\u{0000}", textTxt]
        TEnterComment comment -> 
            let
                commentTxt = comment
            in
                tConcat ["TEnterComment\u{0000}", commentTxt]
        TPostComment comment -> 
            let
                commentTxt = comment
            in
                tConcat ["TPostComment\u{0000}", commentTxt]


--extra types encoders
encodeArticle : Article -> String
encodeArticle article = 
    case article of
        Article title author timestamp body -> 
            let
                titleTxt = title
                authorTxt = author
                timestampTxt = encodeInt 0 999999999 timestamp
                bodyTxt = body
            in
                tConcat ["Article\u{0000}", titleTxt,"\u{0000}",authorTxt,"\u{0000}",timestampTxt,"\u{0000}",bodyTxt]
        Letter title author timestamp body -> 
            let
                titleTxt = title
                authorTxt = author
                timestampTxt = encodeInt 0 999999999 timestamp
                bodyTxt = body
            in
                tConcat ["Letter\u{0000}", titleTxt,"\u{0000}",authorTxt,"\u{0000}",timestampTxt,"\u{0000}",bodyTxt]


encodeDraft : Draft -> String
encodeDraft draft = 
    case draft of
        DraftArticle title author timestamp body comments -> 
            let
                titleTxt = title
                authorTxt = author
                timestampTxt = encodeInt 0 999999999 timestamp
                bodyTxt = body
                commentsTxt =
                    let
                        encodecomments_ _ (str4,commentsList) =
                            case commentsList of
                                uidComment :: rest ->
                                    let
                                        uidCommentTxt =
                                            let
                                                (uid,comment) = uidComment
                                                uidTxt = encodeInt 0 99999 uid
                                                commentTxt = comment
                                            in
                                                tConcat [uidTxt,"\u{0000}",commentTxt]
                                    in
                                        (tConcat [str4,"\u{0000}",uidCommentTxt], rest)
                                [] -> (str4,commentsList)
                        encodecomments ls =
                            lFoldl encodecomments_ ("",ls) (lRange 0 (lLength comments))
                    in
                        tConcat [encodeInt 0 16777216 <| lLength comments, pFst <| encodecomments comments]
            in
                tConcat ["DraftArticle\u{0000}", titleTxt,"\u{0000}",authorTxt,"\u{0000}",timestampTxt,"\u{0000}",bodyTxt,"\u{0000}",commentsTxt]
        DraftLetter title author timestamp body comments -> 
            let
                titleTxt = title
                authorTxt = author
                timestampTxt = encodeInt 0 999999999 timestamp
                bodyTxt = body
                commentsTxt =
                    let
                        encodecomments_ _ (str4,commentsList) =
                            case commentsList of
                                uidComment :: rest ->
                                    let
                                        uidCommentTxt =
                                            let
                                                (uid,comment) = uidComment
                                                uidTxt = encodeInt 0 99999 uid
                                                commentTxt = comment
                                            in
                                                tConcat [uidTxt,"\u{0000}",commentTxt]
                                    in
                                        (tConcat [str4,"\u{0000}",uidCommentTxt], rest)
                                [] -> (str4,commentsList)
                        encodecomments ls =
                            lFoldl encodecomments_ ("",ls) (lRange 0 (lLength comments))
                    in
                        tConcat [encodeInt 0 16777216 <| lLength comments, pFst <| encodecomments comments]
            in
                tConcat ["DraftLetter\u{0000}", titleTxt,"\u{0000}",authorTxt,"\u{0000}",timestampTxt,"\u{0000}",bodyTxt,"\u{0000}",commentsTxt]



