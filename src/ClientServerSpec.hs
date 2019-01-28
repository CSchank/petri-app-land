{-# LANGUAGE OverloadedStrings #-}

module ClientServerSpec where

import Types
import Data.Map as M
import TypeHelpers

outputDirectory = "NewYouthHack"

timestampET doc = edt (ElmIntRange 1548438211 9999999999) "timestamp" doc

articleType :: ElmCustom
articleType = ec -- helper to make custom types
                "Article" -- name of type (Elm syntax rules)
                [("Article",[edt ElmString "title"{-valid function name, used in helper functions-} "title of the article"
                            ,edt ElmString "author" "author"
                            ,timestampET "publication time"
                            ,edt ElmString "body" "article body"
                            ]
                 )
                ,("Letter",[edt ElmString "title" "title of the article being referred to"
                            ,edt ElmString "author" "author"
                            ,timestampET "publication time"
                            ,edt ElmString "body" "article body"
                            ]
                 )
                ]
draftType :: ElmCustom
draftType = ec -- helper to make custom types
                "Draft" -- name of type (Elm syntax rules)
                [("DraftArticle",  [edt ElmString "title"{-valid function name, used in helper functions-} "title of the article"
                            ,edt ElmString "author" "author"
                            ,timestampET "time of last edit"
                            ,edt ElmString "body" "article body"
                            ,edt (ElmList $ edt (ElmPair (edt (ElmIntRange 0 99999) "uid" "userid")
                                                         (edt ElmString "comment" "comment")
                                                )
                                                "uidComment" "(uid,comment)"
                                  ) "comments" "[(uid,comment)]"
                            ]
                 )
                ,("DraftLetter",  [edt ElmString "title"{-valid function name, used in helper functions-} "title of the article"
                            ,edt ElmString "author" "author"
                            ,timestampET "time of last edit"
                            ,edt ElmString "body" "article body"
                            ,edt (ElmList $ edt (ElmPair (edt (ElmIntRange 0 99999) "uid" "userid")
                                                         (edt ElmString "comment" "comment")
                                                )
                                                "uidComment" "(uid,comment)"
                                  ) "comments" "[(uid,comment)]"
                            ]
                 )
                ]
noticeType :: ElmCustom
noticeType = ec -- helper to make custom types
                "Notice" -- name of type (Elm syntax rules)
                [("Notice",  [edt ElmString "title"{-valid function name, used in helper functions-} "short description of the notice"
                            ,edt ElmString "contact" "person to contact for more information"
                            ,edt ElmString "location" "preferably an address which will be searchable in maps"
                            ,timestampET "time the notice was posted (no expiry date)"
                            ,edt ElmString "body" "article body"
                            ,edt (ElmList $ edt (ElmPair (edt (ElmIntRange 0 99999) "uid" "userid")
                                                            (edt ElmString "comment" "comment")
                                                )
                                                "uidComment" "(uid,comment)"
                                    ) "comments" "public "
                            ]
                    )
                ]

clientID = edt (ElmIntRange 0 999999) "clientID" "id assigned when logging in"-- almost a type alias

newYouthNet :: Net
newYouthNet =
    let
        squareOne = 
            HybridPlace "SquareOne" 
                [edt (ElmList $ edt (ElmType "Notice") "notice" "") "notices" ""] --server state
                [] --player state
                [] --client state
                Nothing
                (Nothing, Nothing)
                Nothing

        universities = 
            HybridPlace "UniversitiesAndColleges" 
                [edt (ElmList $ edt (ElmType "Notice") "notice" "") "notices" ""] --server state
                [clientID] --player state
                [] --client state
                Nothing
                (Nothing, Nothing)
                Nothing

        mcmaster = 
            HybridPlace "McMasterUniversity" 
                [edt (ElmList $ edt (ElmType "Notice") "notice" "") "notices" ""
                ,edt (ElmList $ edt (ElmTriple (edt (ElmIntRange 0 99999) "uid" "userid") 
                                                (timestampET "time posted")
                                                (edt ElmString "body" "public statement")) "statement" "") "statements" ""
                ] --server state
                [clientID] --player state
                [edt (ElmList $ edt (ElmType "Notice") "notice" "") "notices" ""
                ,edt (ElmList $ edt (ElmTriple (edt (ElmIntRange 0 99999) "uid" "userid") 
                                                (timestampET "time posted")
                                                (edt ElmString "body" "public statement")) "statement" "") "statements" ""] --client state
                Nothing
                (Nothing, Nothing)
                Nothing

        mcmasterNoticeRoom = 
            HybridPlace "McMasterCreateNotice" 
                [] --server state
                [clientID] --player state
                [edt (ElmType "Notice") "notice" ""] --client state
                Nothing
                (Nothing, Nothing)
                Nothing
{-}
        mcmasterStudenCentre = 
            HybridPlace "McMasterStudentCentre" 
                [edt (ElmList $ edt (ElmType "Notice") "notice" "") "notices" ""] --server state
                [clientID] --player state
                [] --client state
                Nothing
                (Nothing, Nothing)
                Nothing
  -}      
                        
        mcmasterCreateNotice =
            NetTransition
                (constructor "EnterMcMasterCreateNotice" [])
                [("McMasterUniversity", Just ("McMasterCreateNotice", constructor "DidEnterMcMasterCreateNotice" [])) -- the user's client gets this
                ,("McMasterUniversity", Nothing)] -- everyone else gets this
                Nothing
        editMcMasterNotice =
            NetTransition
                (constructor "EditMcMasterNotice" [edt ElmString "partialNotice" "notice after every keystroke"])
                [("McMasterCreateNotice", Just ("McMasterCreateNotice", constructor "DidEditMcMasterNotice" []))
                ,("McMasterCreateNotice", Nothing)]
                Nothing
        publishMcMasterNotice =                 
            NetTransition
                (constructor "PublishMcMasterNotice" [edt (ElmType "Notice") "notice" "new notice"])
                [("PublishMcMasterNotice", Just ("McMasterUniversity", constructor "DidPublishMcMasterNotice" [edt (ElmType "Notice") "notice" "new notice"]))
                ,("PublishMcMasterNotice", Nothing)
                ,("McMasterUniversity", Just ("McMasterUniversity", constructor "NewMcMasterNotice" [edt (ElmType "Notice") "notice" "new notice"]))]
                Nothing
        mcmasterCancelNotice =
            NetTransition
                (constructor "CancelNotice" [])
                [("McMasterCreateNotice", Just ("McMasterUniversity", constructor "DidCancelNotice" []))
                ,("McMasterCreateNotice", Nothing)] -- if you are not editing, then go back to the same place
                Nothing

        enterUniversities =
            NetTransition
                (constructor "EnterUniversities" [])
                [("SquareOne", Just ("UniversitiesAndColleges", constructor "DidEnterUniversitiesAndColleges" []))
                ,("SquareOne", Nothing)]
                Nothing
        universitiesToSquareOne =
            NetTransition
                (constructor "ExitUniversities" [])
                [("UniversitiesAndColleges", Just ("SquareOne", constructor "DidLeaveUniversities" []))
                ,("UniversitiesAndColleges", Nothing)]
                Nothing
        enterMcMaster =
            NetTransition
                (constructor "EnterMcMasterUniversity" [])
                [("UniversitiesAndColleges", Just ("McMasterUniversity", constructor "DidEnterMcMasterUniversity" []))
                ,("UniversitiesAndColleges", Nothing)]
                Nothing
        exitMcMaster =
            NetTransition
                (constructor "ExitMcMasterUniversity" [])
                [("McMasterUniversity", Just ("UniversitiesAndColleges", constructor "DidLeaveMcMasterUniversity" []))
                ,("McMasterUniversity", Nothing)]
                Nothing
        editMcMasterComment =
            NetTransition
                (constructor "EditMcMasterComment" [edt ElmString "partialComment" "comment after every keystroke"])
                [("McMasterUniversity", Just ("McMasterUniversity", constructor "DidEditMcMasterComment" []))
                ,("McMasterUniversity", Nothing)]
                Nothing
        sendMcMasterComment =                 
            NetTransition
                (constructor "SendMcMasterComment" [edt ElmString "comment" "comment as it will be sent"])
                [("McMasterUniversity", Just ("McMasterUniversity", constructor "DidSendMcMasterComment" [edt ElmString "comment" "comment as it will be sent"]))]
                Nothing
    in
        HybridNet
            "NewYouthHack"
            "SquareOne"
            [squareOne, universities, mcmaster, mcmasterNoticeRoom]
            [(ClientOnlyTransition,mcmasterCreateNotice),(ClientOnlyTransition,mcmasterCancelNotice)
            ,(ClientOnlyTransition,enterUniversities),(ClientOnlyTransition,universitiesToSquareOne),(ClientOnlyTransition,enterMcMaster)
            ,(ClientOnlyTransition,exitMcMaster),(ClientOnlyTransition,editMcMasterComment),(ClientOnlyTransition,sendMcMasterComment)
            ,(ClientOnlyTransition,editMcMasterNotice),(ClientOnlyTransition,publishMcMasterNotice)]
            []


clientServerApp :: ClientServerApp
clientServerApp =
    ( "NewYouthHack"           --starting net for a client
    , [newYouthNet]           --all the nets in this client/server app
    , [noticeType]                  --extra client types used in states or messages
    , [noticeType]                  --extra server types used in states or messages
    )