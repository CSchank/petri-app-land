{-# LANGUAGE OverloadedStrings #-}

module ClientServerSpec where

import Types
import Data.Map as M
import TypeHelpers

outputDirectory = "NewspaperExample"

articleType :: CustomT
articleType = ec -- helper to make custom types
                "Article" -- name of type (Elm syntax rules)
                [("Article",[edt StringT "title"{-valid function name, used in helper functions-} "title of the article"
                            ,edt StringT "author" "author"
                            ,edt (IntRangeT 0 999999999) "timestamp" "seconds since 1970" -- warning Y2.286K bug
                            ,edt StringT "body" "article body"
                            ]
                 )
                ,("Letter",[edt StringT "title" "title of the article being referred to"
                            ,edt StringT "author" "author"
                            ,edt (IntRangeT 0 999999999) "timestamp" "seconds since 1970" -- warning Y2.286K bug
                            ,edt StringT "body" "article body"
                            ]
                 )
                ]
draftType :: CustomT
draftType = ec -- helper to make custom types
                "Draft" -- name of type (Elm syntax rules)
                [("DraftArticle",  [edt StringT "title"{-valid function name, used in helper functions-} "title of the article"
                            ,edt StringT "author" "author"
                            ,edt (IntRangeT 0 999999999) "timestamp" "seconds since 1970" -- warning Y2.286K bug
                            ,edt StringT "body" "article body"
                            ,edt (ListT $ edt (PairT (edt (IntRangeT 0 99999) "uid" "userid")
                                                         (edt StringT "comment" "comment")
                                                )
                                                "uidComment" "(uid,comment)"
                                  ) "comments" "[(uid,comment)]"
                            ]
                 )
                ,("DraftLetter",  [edt StringT "title"{-valid function name, used in helper functions-} "title of the article"
                            ,edt StringT "author" "author"
                            ,edt (IntRangeT 0 999999999) "timestamp" "seconds since 1970" -- warning Y2.286K bug
                            ,edt StringT "body" "article body"
                            ,edt (ListT $ edt (PairT (edt (IntRangeT 0 99999) "uid" "userid")
                                                         (edt StringT "comment" "comment")
                                                )
                                                "uidComment" "(uid,comment)"
                                  ) "comments" "[(uid,comment)]"
                            ]
                 )
                ]
newspaperNet :: Net
newspaperNet =
    let
        mainStreet = 
            HybridPlace "MainStreet" 
                [] --server state
                [] --player state
                [] --client state
                Nothing
                (Nothing, Nothing)
                Nothing

        readingRoom = 
            HybridPlace "ReadingRoom" 
                [edt (ListT $ edt (TypeT "Article") "article" "") "articles" ""] --server state
                [edt StringT "nowReading" "title of current article being read"] --player state
                [edt (ListT $ edt (TypeT "Article") "article" "") "articles" "" -- partial list of articles
                ,edt (ListT $ edt StringT "title" "") "titles" "" -- all article titles
                ,edt (MaybeT (edt StringT "viewing" "title of article begin viewed")) "maybeViewing" "article being viewed or Nothing for index"] --client state
                Nothing
                (Nothing, Nothing)
                Nothing

        editingRoom = 
            HybridPlace "EditingRoom" 
                [edt (ListT $ edt (TypeT "Draft") "drafts" "") "articles" ""] --server state
                [edt (MaybeT (edt StringT "nowEditing" "title of current article being read")) "maybeEditing" "article being edited or Nothing for index"] --player state
                [edt (MaybeT (edt (TypeT "Draft") "article" "article currently being edited")) "maybeEditing" "article being edited or Nothing for index"
                ,edt (ListT $ edt StringT "title" "") "titles" "" -- all article titles
                ] --client state
                Nothing
                (Nothing, Nothing)
                Nothing
        enterRR =
            Transition
                (constructor "EnterReadingRoom" [])
                [("MainStreet", Just ("ReadingRoom", constructor "DidEnterReadingRoom" [edt (ListT $ edt (TypeT "Article") "article" "") "articles" ""]))]
                Nothing
        enterER =
            Transition
                (constructor "EnterEditingRoom" [])
                [("MainStreet", Just ("ReadingRoom", constructor "DidEnterEditingRoom" [edt (ListT $ edt StringT "title" "") "articles" ""]))]
                Nothing
        startEditing =
            Transition
                (constructor "StartEditing" [edt StringT "title" "article to start editing"])
                [("EditingRoom", Just ("EditingRoom", constructor "DidStartEditing" [edt (TypeT "Draft") "draft" "article to edit"]))]
                Nothing
        leaveRR =
            Transition
                (constructor "LeaveReadingRoom" [])
                [("ReadingRoom", Just ("MainStreet", constructor "DidLeaveReadingRoom" []))]
                Nothing
        leaveER =
            Transition
                (constructor "LeaveEditingRoom" [])
                [("EditingRoom", Just ("MainStreet", constructor "DidLeaveEditingRoom" []))]
                Nothing
        publishArticle =
            Transition
                (constructor "PublishArticle" [])
                [("EditingRoom", Just ("ReadingRoom", constructor "DidPublish" [edt (ListT $ edt (TypeT "Article") "article" "") "articles" ""]))
                ,("EditingRoom", Nothing)] -- if you are not editing, then go back to the same place
                Nothing
        saveDraft =
            Transition
                (constructor "SaveDraft" [edt (TypeT "Draft") "draft" "edited draft"])
                [("EditingRoom", Just ("EditingRoom", constructor "DidSaveDraft" [edt (ListT $ edt StringT "article" "article title") "articles" "titles of all drafts"]))
                ] -- if you are not editing, then go back to the same place
                Nothing
        enterTitle =
            Transition
                (constructor "EnterTitle" [edt StringT "title" "edited title"])
                [("EditingRoom", Just ("EditingRoom", constructor "DidEnterTitle" [edt (ListT $ edt StringT "article" "article title") "articles" "titles of all drafts"]))
                ] -- if you are not editing, then go back to the same place
                Nothing
        enterText =
            Transition
                (constructor "EnterText" [edt StringT "text" "edited text"])
                [("EditingRoom", Just ("EditingRoom", constructor "DidEnterText" [edt (ListT $ edt StringT "article" "article title") "articles" "titles of all drafts"]))
                ] -- if you are not editing, then go back to the same place
                Nothing
        enterComment =
            Transition
                (constructor "EnterComment" [edt StringT "comment" "edited comment"])
                [("EditingRoom", Just ("EditingRoom", constructor "DidEnterComment" [edt StringT "comment" "edited comment"]))
                ] -- if you are not editing, then go back to the same place
                Nothing
        postComment =
            Transition
                (constructor "PostComment" [edt StringT "comment" "edited comment"])
                [("EditingRoom", Just ("EditingRoom", constructor "DidPostComment" [edt StringT "comment" "edited comment"]))
                ] -- if you are not editing, then go back to the same place
                Nothing
    in
        HybridNet
            "NewspaperExample"
            "MainStreet"
            [mainStreet,readingRoom,editingRoom]
            [(HybridTransition,enterRR),(HybridTransition,enterER),(HybridTransition,startEditing),(HybridTransition,leaveRR),(HybridTransition,leaveER),(HybridTransition,publishArticle),(HybridTransition,saveDraft),(HybridTransition,enterTitle),(HybridTransition,enterText),(HybridTransition,enterComment),(HybridTransition,postComment)]
            []


clientServerApp :: ClientServerApp
clientServerApp =
    ( "NewspaperExample"           --starting net for a client
    , [newspaperNet]           --all the nets in this client/server app
    , [articleType,draftType]                  --extra client types used in states or messages
    , [articleType,draftType]                  --extra server types used in states or messages
    )