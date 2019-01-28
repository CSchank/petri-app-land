{-# LANGUAGE OverloadedStrings #-}
module NewYouthHack.Static.Encode where
import NewYouthHack.Static.Types

import Utils.Utils
import qualified Data.Text as T
import Static.Types
encodeClientMessage :: ClientMessage -> T.Text
encodeClientMessage clientmessage = 
    case clientmessage of
        MDidEnterMcMasterCreateNotice ->                 tConcat ["MDidEnterMcMasterCreateNotice\0"]
        MDidCancelNotice ->                 tConcat ["MDidCancelNotice\0"]
        MDidEnterUniversitiesAndColleges ->                 tConcat ["MDidEnterUniversitiesAndColleges\0"]
        MDidLeaveUniversities ->                 tConcat ["MDidLeaveUniversities\0"]
        MDidEnterMcMasterUniversity ->                 tConcat ["MDidEnterMcMasterUniversity\0"]
        MDidLeaveMcMasterUniversity ->                 tConcat ["MDidLeaveMcMasterUniversity\0"]
        MDidEditMcMasterComment ->                 tConcat ["MDidEditMcMasterComment\0"]
        MDidSendMcMasterComment comment -> 
            let
                commentTxt = T.pack comment
            in
                tConcat ["MDidSendMcMasterComment\0", commentTxt]
        MDidEditMcMasterNotice ->                 tConcat ["MDidEditMcMasterNotice\0"]
        MDidPublishMcMasterNotice notice -> 
            let
                noticeTxt = encodeNotice notice
            in
                tConcat ["MDidPublishMcMasterNotice\0", noticeTxt]
        MNewMcMasterNotice notice -> 
            let
                noticeTxt = encodeNotice notice
            in
                tConcat ["MNewMcMasterNotice\0", noticeTxt]


-- extra type encoders
encodeNotice :: Notice -> T.Text
encodeNotice notice = 
    case notice of
        Notice title contact location timestamp body comments -> 
            let
                titleTxt = T.pack title
                contactTxt = T.pack contact
                locationTxt = T.pack location
                timestampTxt = encodeInt 1548438211 9999999999 timestamp
                bodyTxt = T.pack body
                commentsTxt =
                    let
                        encodecomments_ _ (str4,commentsList) =
                            case commentsList of
                                uidComment : rest ->
                                    let
                                        uidCommentTxt =
                                            let
                                                (uid,comment) = uidComment
                                                uidTxt = encodeInt 0 99999 uid
                                                commentTxt = T.pack comment
                                            in
                                                tConcat [uidTxt,"\0",commentTxt]
                                    in
                                        (tConcat [str4,"\0",uidCommentTxt], rest)
                                [] -> (str4,commentsList)
                        encodecomments ls =
                            lFoldl encodecomments_ ("",ls) (lRange 0 (lLength comments))
                    in
                        tConcat [encodeInt 0 16777216 <| lLength comments, pFst <| encodecomments comments]
            in
                tConcat ["Notice\0", titleTxt,"\0",contactTxt,"\0",locationTxt,"\0",timestampTxt,"\0",bodyTxt,"\0",commentsTxt]



