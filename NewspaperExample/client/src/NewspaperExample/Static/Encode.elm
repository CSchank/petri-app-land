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


