module NewYouthHack.Static.Encode exposing(..)
import NewYouthHack.Static.Types exposing(..)

import Utils.Utils exposing(..)
import Static.Types
encodeOutgoingTransition : OutgoingTransition -> String
encodeOutgoingTransition outgoingtransition = 
    case outgoingtransition of
        TEnterMcMasterCreateNotice ->                 tConcat ["TEnterMcMasterCreateNotice\u{0000}"]
        TCancelNotice ->                 tConcat ["TCancelNotice\u{0000}"]
        TEnterUniversities ->                 tConcat ["TEnterUniversities\u{0000}"]
        TExitUniversities ->                 tConcat ["TExitUniversities\u{0000}"]
        TEnterMcMasterUniversity ->                 tConcat ["TEnterMcMasterUniversity\u{0000}"]
        TExitMcMasterUniversity ->                 tConcat ["TExitMcMasterUniversity\u{0000}"]
        TEditMcMasterComment partialComment -> 
            let
                partialCommentTxt = partialComment
            in
                tConcat ["TEditMcMasterComment\u{0000}", partialCommentTxt]
        TSendMcMasterComment comment -> 
            let
                commentTxt = comment
            in
                tConcat ["TSendMcMasterComment\u{0000}", commentTxt]
        TEditMcMasterNotice partialNotice -> 
            let
                partialNoticeTxt = partialNotice
            in
                tConcat ["TEditMcMasterNotice\u{0000}", partialNoticeTxt]
        TPublishMcMasterNotice notice -> 
            let
                noticeTxt = encodeNotice notice
            in
                tConcat ["TPublishMcMasterNotice\u{0000}", noticeTxt]


--extra types encoders
encodeNotice : Notice -> String
encodeNotice notice = 
    case notice of
        Notice title contact location timestamp body comments -> 
            let
                titleTxt = title
                contactTxt = contact
                locationTxt = location
                timestampTxt = encodeInt 1548438211 9999999999 timestamp
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
                tConcat ["Notice\u{0000}", titleTxt,"\u{0000}",contactTxt,"\u{0000}",locationTxt,"\u{0000}",timestampTxt,"\u{0000}",bodyTxt,"\u{0000}",commentsTxt]



