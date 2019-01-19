{-# LANGUAGE OverloadedStrings #-}

module ClientServerSpec where

import Types
import Data.Map as M
import TypeHelpers

outputDirectory = "NewspaperExample"

articleType :: ElmCustom
articleType = ec -- helper to make custom types
                "Article" -- name of type (Elm syntax rules)
                [("Article",[edt ElmString "title"{-valid function name, used in helper functions-} "title of the article"
                            ,edt ElmString "author" "author"
                            ,edt (ElmIntRange 0 999999999) "timestamp" "seconds since 1970" -- warning Y2.286K bug
                            ,edt ElmString "body" "article body"
                            ]
                 )
                ,("Letter",[edt ElmString "title" "title of the article being referred to"
                            ,edt ElmString "author" "author"
                            ,edt (ElmIntRange 0 999999999) "timestamp" "seconds since 1970" -- warning Y2.286K bug
                            ,edt ElmString "body" "article body"
                            ]
                 )
                ]
draftType :: ElmCustom
draftType = ec -- helper to make custom types
                "Draft" -- name of type (Elm syntax rules)
                [("DraftArticle",  [edt ElmString "title"{-valid function name, used in helper functions-} "title of the article"
                            ,edt ElmString "author" "author"
                            ,edt (ElmIntRange 0 999999999) "timestamp" "seconds since 1970" -- warning Y2.286K bug
                            ,edt ElmString "body" "article body"
                            ,edt (ElmList $ edt (ElmPair (edt (ElmIntRange 0 99999) "uid" "userid") (edt ElmString "comment" "comment"))) "comments" "[(uid,comment)]"
                            ]
                 )
                ,("DraftLetter",  [edt ElmString "title"{-valid function name, used in helper functions-} "title of the article"
                            ,edt ElmString "author" "author"
                            ,edt (ElmIntRange 0 999999999) "timestamp" "seconds since 1970" -- warning Y2.286K bug
                            ,edt ElmString "body" "article body"
                            ,edt (ElmList $ edt (ElmPair (edt (ElmIntRange 0 99999) "uid" "userid") (edt ElmString "comment" "comment"))) "comments" "[(uid,comment)]"
                            ]
                 )
                ]
testNet :: Net
testNet =
    let
        place1 = 
            HybridPlace "MainStreet" 
                [] --server state
                [] --player state
                [] --client state
                Nothing
                (Nothing, Nothing)
                Nothing

        place2 = 
            HybridPlace "ReadingRoom" 
                [edt (ElmList $ edt (ElmType "Article") "article" "") "articles" ""] --server state
                [edt ElmString "nowReading" "title of current article being read"] --player state
                [edt (ElmList $ edt (ElmType "Article") "article" "") "articles" "" -- partial list of articles
                ,edt (ElmList $ edt ElmString "title" "") "titles" "" -- all article titles
                ,edt (ElmMaybe (edt ElmString "viewing" "title of article begin viewed")) "maybeViewing" "article being viewed or Nothing for index"] --client state
                Nothing
                (Nothing, Nothing)
                Nothing

        place3 = 
            HybridPlace "EditingRoom" 
                [edt (ElmList $ edt (ElmType "Draft") "drafts" "") "articles" ""] --server state
                [edt (ElmMaybe (edt ElmString "nowEditing" "title of current article being read")) "maybeEditing" "article being edited or Nothing for index"] --player state
                [edt (ElmMaybe (edt (ElmType "Draft") "article" "article currently being edited") "maybeEditing" "article being edited or Nothing for index"
                ,edt (ElmList $ edt ElmString "title" "") "titles" "" -- all article titles
                ] --client state
                Nothing
                (Nothing, Nothing)
                Nothing
        enterRR =
            NetTransition
                (constructor "EnterReadingRoom" [])
                [("MainStreet", ("ReadingRoom", Just $ constructor "DidEnterReadingRoom" [edt (ElmList $ edt (ElmType "Article") "article" "") "articles" ""]))]
                Nothing
        enterER =
            NetTransition
                (constructor "EnterEditingRoom" [])
                [("MainStreet", ("ReadingRoom", Just $ constructor "DidEnterEditingRoom" [edt (ElmList $ edt ElmString "title" "") "articles" ""]))]
                Nothing
        startEditing =
            NetTransition
                (constructor "StartEditing" [edt ElmString "title" "article to start editing"])
                [("EditingRoom", ("EditingRoom", Just $ constructor "DidStartEditing" [edt (ElmType "Draft") "draft" "article to edit"]))]
                Nothing
        leaveRR =
            NetTransition
                (constructor "LeaveReadingRoom" [])
                [("ReadingRoom", ("MainStreet", Just $ constructor "DidLeaveReadingRoom" []))]
                Nothing
        leaveER =
            NetTransition
                (constructor "LeaveEditingRoom" [])
                [("EditingRoom", ("MainStreet", Just $ constructor "DidLeaveEditingRoom" []))]
                Nothing
        publishArticle =
            NetTransition
                (constructor "PublishArticle" [])
                [("EditingRoom", ("ReadingRoom", Just $ constructor "DidPublish" [edt (ElmList $ edt (ElmType "Article") "article" "") "articles" ""]))
                ,("EditingRoom", ("EditingRoom", Nothing))] -- if you are not editing, then go back to the same place
                Nothing
        saveDraft =
            NetTransition
                (constructor "SaveDraft" [edt (ElmType "Draft") "draft" "edited draft"])
                [("EditingRoom", ("EditingRoom", Just $ constructor "DidSaveDraft" [edt (ElmList $ edt ElmString "article" "article title") "articles" "titles of all drafts"]))
                ] -- if you are not editing, then go back to the same place
                Nothing
        enterTitle =
            NetTransition
                (constructor "EnterTitle" [edt ElmString "title" "edited title"])
                [("EditingRoom", ("EditingRoom", Just $ constructor "DidEnterTitle" [edt (ElmList $ edt ElmString "article" "article title") "articles" "titles of all drafts"]))
                ] -- if you are not editing, then go back to the same place
                Nothing
        enterText =
            NetTransition
                (constructor "EnterText" [edt ElmString "text" "edited text"])
                [("EditingRoom", ("EditingRoom", Just $ constructor "DidEnterText" [edt (ElmList $ edt ElmString "article" "article title") "articles" "titles of all drafts"]))
                ] -- if you are not editing, then go back to the same place
                Nothing
        enterComment =
            NetTransition
                (constructor "EnterComment" [edt ElmString "comment" "edited comment"])
                [("EditingRoom", ("EditingRoom", Just $ constructor "DidEnterComment" [edt ElmString "comment" "edited comment"]))
                ] -- if you are not editing, then go back to the same place
                Nothing
        postComment =
            NetTransition
                (constructor "PostComment" [edt ElmString "comment" "edited comment"])
                [("EditingRoom", ("EditingRoom", Just $ constructor "DidEnterComment" [edt ElmString "comment" "edited comment"]))
                ] -- if you are not editing, then go back to the same place
                Nothing
    in
        HybridNet
            "TestNet"
            "A"
            [place1,place2,place3]
            [(ServerOnlyTransition,trans1),(ClientOnlyTransition,trans2),(HybridTransition,trans3)]
            []


clientServerApp :: ClientServerApp
clientServerApp =
    ( "TestNet"           --starting net for a client
    , [testNet]           --all the nets in this client/server app
    , []                  --extra client types used in states or messages
    , []                  --extra server types used in states or messages
    )