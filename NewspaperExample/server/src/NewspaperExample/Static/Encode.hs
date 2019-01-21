{-# LANGUAGE OverloadedStrings #-}
module NewspaperExample.Static.Encode where
import NewspaperExample.Static.Types

import Utils.Utils
import qualified Data.Text as T
import Static.Types
encodeClientMessage :: ClientMessage -> T.Text
encodeClientMessage clientmessage = 
    case clientmessage of
        MDidEnterReadingRoom articles -> 
            let
                articlesTxt =
                    let
                        encodearticles_ _ (str4,articlesList) =
                            case articlesList of
                                article : rest ->
                                    let
                                        articleTxt = encodeArticle article
                                    in
                                        (tConcat [str4,"\0",articleTxt], rest)
                                [] -> (str4,articlesList)
                        encodearticles ls =
                            lFoldl encodearticles_ ("",ls) (lRange 0 (lLength articles))
                    in
                        tConcat [encodeInt 0 16777216 <| lLength articles, pFst <| encodearticles articles]
            in
                tConcat ["MDidEnterReadingRoom\0", articlesTxt]
        MDidEnterEditingRoom articles -> 
            let
                articlesTxt =
                    let
                        encodearticles_ _ (str4,articlesList) =
                            case articlesList of
                                title : rest ->
                                    let
                                        titleTxt = T.pack title
                                    in
                                        (tConcat [str4,"\0",titleTxt], rest)
                                [] -> (str4,articlesList)
                        encodearticles ls =
                            lFoldl encodearticles_ ("",ls) (lRange 0 (lLength articles))
                    in
                        tConcat [encodeInt 0 16777216 <| lLength articles, pFst <| encodearticles articles]
            in
                tConcat ["MDidEnterEditingRoom\0", articlesTxt]
        MDidStartEditing draft -> 
            let
                draftTxt = encodeDraft draft
            in
                tConcat ["MDidStartEditing\0", draftTxt]
        MDidLeaveReadingRoom ->                 tConcat ["MDidLeaveReadingRoom\0"]
        MDidLeaveEditingRoom ->                 tConcat ["MDidLeaveEditingRoom\0"]
        MDidPublish articles -> 
            let
                articlesTxt =
                    let
                        encodearticles_ _ (str4,articlesList) =
                            case articlesList of
                                article : rest ->
                                    let
                                        articleTxt = encodeArticle article
                                    in
                                        (tConcat [str4,"\0",articleTxt], rest)
                                [] -> (str4,articlesList)
                        encodearticles ls =
                            lFoldl encodearticles_ ("",ls) (lRange 0 (lLength articles))
                    in
                        tConcat [encodeInt 0 16777216 <| lLength articles, pFst <| encodearticles articles]
            in
                tConcat ["MDidPublish\0", articlesTxt]
        MDidSaveDraft articles -> 
            let
                articlesTxt =
                    let
                        encodearticles_ _ (str4,articlesList) =
                            case articlesList of
                                article : rest ->
                                    let
                                        articleTxt = T.pack article
                                    in
                                        (tConcat [str4,"\0",articleTxt], rest)
                                [] -> (str4,articlesList)
                        encodearticles ls =
                            lFoldl encodearticles_ ("",ls) (lRange 0 (lLength articles))
                    in
                        tConcat [encodeInt 0 16777216 <| lLength articles, pFst <| encodearticles articles]
            in
                tConcat ["MDidSaveDraft\0", articlesTxt]
        MDidEnterTitle articles -> 
            let
                articlesTxt =
                    let
                        encodearticles_ _ (str4,articlesList) =
                            case articlesList of
                                article : rest ->
                                    let
                                        articleTxt = T.pack article
                                    in
                                        (tConcat [str4,"\0",articleTxt], rest)
                                [] -> (str4,articlesList)
                        encodearticles ls =
                            lFoldl encodearticles_ ("",ls) (lRange 0 (lLength articles))
                    in
                        tConcat [encodeInt 0 16777216 <| lLength articles, pFst <| encodearticles articles]
            in
                tConcat ["MDidEnterTitle\0", articlesTxt]
        MDidEnterText articles -> 
            let
                articlesTxt =
                    let
                        encodearticles_ _ (str4,articlesList) =
                            case articlesList of
                                article : rest ->
                                    let
                                        articleTxt = T.pack article
                                    in
                                        (tConcat [str4,"\0",articleTxt], rest)
                                [] -> (str4,articlesList)
                        encodearticles ls =
                            lFoldl encodearticles_ ("",ls) (lRange 0 (lLength articles))
                    in
                        tConcat [encodeInt 0 16777216 <| lLength articles, pFst <| encodearticles articles]
            in
                tConcat ["MDidEnterText\0", articlesTxt]
        MDidEnterComment comment -> 
            let
                commentTxt = T.pack comment
            in
                tConcat ["MDidEnterComment\0", commentTxt]
        MDidPostComment comment -> 
            let
                commentTxt = T.pack comment
            in
                tConcat ["MDidPostComment\0", commentTxt]


-- extra type encoders
encodeArticle :: Article -> T.Text
encodeArticle article = 
    case article of
        Article title author timestamp body -> 
            let
                titleTxt = T.pack title
                authorTxt = T.pack author
                timestampTxt = encodeInt 0 999999999 timestamp
                bodyTxt = T.pack body
            in
                tConcat ["Article\0", titleTxt,"\0",authorTxt,"\0",timestampTxt,"\0",bodyTxt]
        Letter title author timestamp body -> 
            let
                titleTxt = T.pack title
                authorTxt = T.pack author
                timestampTxt = encodeInt 0 999999999 timestamp
                bodyTxt = T.pack body
            in
                tConcat ["Letter\0", titleTxt,"\0",authorTxt,"\0",timestampTxt,"\0",bodyTxt]


encodeDraft :: Draft -> T.Text
encodeDraft draft = 
    case draft of
        DraftArticle title author timestamp body comments -> 
            let
                titleTxt = T.pack title
                authorTxt = T.pack author
                timestampTxt = encodeInt 0 999999999 timestamp
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
                tConcat ["DraftArticle\0", titleTxt,"\0",authorTxt,"\0",timestampTxt,"\0",bodyTxt,"\0",commentsTxt]
        DraftLetter title author timestamp body comments -> 
            let
                titleTxt = T.pack title
                authorTxt = T.pack author
                timestampTxt = encodeInt 0 999999999 timestamp
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
                tConcat ["DraftLetter\0", titleTxt,"\0",authorTxt,"\0",timestampTxt,"\0",bodyTxt,"\0",commentsTxt]



