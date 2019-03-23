{-# LANGUAGE OverloadedStrings #-}

module ClientServerSpec where

import Types
import Data.Map as M
import TypeHelpers

outputDirectory = "NewspaperExample"

articleType :: CustomT
articleType = ct -- helper to make custom types
                "Article" -- name of type (Elm syntax rules)
                [("Article",[dt StringT "title"{-valid function name, used in helper functions-} "title of the article"
                            ,dt StringT "author" "author"
                            ,dt (IntRangeT 0 999999999) "timestamp" "seconds since 1970" -- warning Y2.286K bug
                            ,dt StringT "body" "article body"
                            ]
                 )
                ,("Letter",[dt StringT "title" "title of the article being referred to"
                            ,dt StringT "author" "author"
                            ,dt (IntRangeT 0 999999999) "timestamp" "seconds since 1970" -- warning Y2.286K bug
                            ,dt StringT "body" "article body"
                            ]
                 )
                ]
draftType :: CustomT
draftType = ct -- helper to make custom types
                "Draft" -- name of type (Elm syntax rules)
                [("DraftArticle",  [dt StringT "title"{-valid function name, used in helper functions-} "title of the article"
                            ,dt StringT "author" "author"
                            ,dt (IntRangeT 0 999999999) "timestamp" "seconds since 1970" -- warning Y2.286K bug
                            ,dt StringT "body" "article body"
                            ,dt (ListT $ dt (PairT (dt (IntRangeT 0 99999) "uid" "userid")
                                                         (dt StringT "comment" "comment")
                                                )
                                                "uidComment" "(uid,comment)"
                                  ) "comments" "[(uid,comment)]"
                            ]
                 )
                ,("DraftLetter",  [dt StringT "title"{-valid function name, used in helper functions-} "title of the article"
                            ,dt StringT "author" "author"
                            ,dt (IntRangeT 0 999999999) "timestamp" "seconds since 1970" -- warning Y2.286K bug
                            ,dt StringT "body" "article body"
                            ,dt (ListT $ dt (PairT (dt (IntRangeT 0 99999) "uid" "userid")
                                                         (dt StringT "comment" "comment")
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
                [dt (ListT $ dt (TypeT "Article") "article" "") "articles" ""] --server state
                [dt StringT "nowReading" "title of current article being read"] --player state
                [dt (ListT $ dt (TypeT "Article") "article" "") "articles" "" -- partial list of articles
                ,dt (ListT $ dt StringT "title" "") "titles" "" -- all article titles
                ,dt (MaybeT (dt StringT "viewing" "title of article begin viewed")) "maybeViewing" "article being viewed or Nothing for index"] --client state
                Nothing
                (Nothing, Nothing)
                Nothing

        editingRoom = 
            HybridPlace "EditingRoom" 
                [dt (ListT $ dt (TypeT "Draft") "drafts" "") "articles" ""] --server state
                [dt (MaybeT (dt StringT "nowEditing" "title of current article being read")) "maybeEditing" "article being edited or Nothing for index"] --player state
                [dt (MaybeT (dt (TypeT "Draft") "article" "article currently being edited")) "maybeEditing" "article being edited or Nothing for index"
                ,dt (ListT $ dt StringT "title" "") "titles" "" -- all article titles
                ] --client state
                Nothing
                (Nothing, Nothing)
                Nothing
        enterRR =
            Transition
                (constructor "EnterReadingRoom" [])
                [("MainStreet", Just ("ReadingRoom", constructor "DidEnterReadingRoom" [dt (ListT $ dt (TypeT "Article") "article" "") "articles" ""]))]
                Nothing
        enterER =
            Transition
                (constructor "EnterEditingRoom" [])
                [("MainStreet", Just ("ReadingRoom", constructor "DidEnterEditingRoom" [dt (ListT $ dt StringT "title" "") "articles" ""]))]
                Nothing
        startEditing =
            Transition
                (constructor "StartEditing" [dt StringT "title" "article to start editing"])
                [("EditingRoom", Just ("EditingRoom", constructor "DidStartEditing" [dt (TypeT "Draft") "draft" "article to edit"]))]
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
                [("EditingRoom", Just ("ReadingRoom", constructor "DidPublish" [dt (ListT $ dt (TypeT "Article") "article" "") "articles" ""]))
                ,("EditingRoom", Nothing)] -- if you are not editing, then go back to the same place
                Nothing
        saveDraft =
            Transition
                (constructor "SaveDraft" [dt (TypeT "Draft") "draft" "edited draft"])
                [("EditingRoom", Just ("EditingRoom", constructor "DidSaveDraft" [dt (ListT $ dt StringT "article" "article title") "articles" "titles of all drafts"]))
                ] -- if you are not editing, then go back to the same place
                Nothing
        enterTitle =
            Transition
                (constructor "EnterTitle" [dt StringT "title" "edited title"])
                [("EditingRoom", Just ("EditingRoom", constructor "DidEnterTitle" [dt (ListT $ dt StringT "article" "article title") "articles" "titles of all drafts"]))
                ] -- if you are not editing, then go back to the same place
                Nothing
        enterText =
            Transition
                (constructor "EnterText" [dt StringT "text" "edited text"])
                [("EditingRoom", Just ("EditingRoom", constructor "DidEnterText" [dt (ListT $ dt StringT "article" "article title") "articles" "titles of all drafts"]))
                ] -- if you are not editing, then go back to the same place
                Nothing
        enterComment =
            Transition
                (constructor "EnterComment" [dt StringT "comment" "edited comment"])
                [("EditingRoom", Just ("EditingRoom", constructor "DidEnterComment" [dt StringT "comment" "edited comment"]))
                ] -- if you are not editing, then go back to the same place
                Nothing
        postComment =
            Transition
                (constructor "PostComment" [dt StringT "comment" "edited comment"])
                [("EditingRoom", Just ("EditingRoom", constructor "DidPostComment" [dt StringT "comment" "edited comment"]))
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