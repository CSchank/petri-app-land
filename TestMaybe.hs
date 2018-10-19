module TestServer where

import Types
import qualified Data.Map as M
import Generate.Server 
import TestElmTypes
import Generate.OneOf

sIdle =   ("Idle",[(ElmMaybe (ElmIntRange 0 255,"n",""),"maybeN","")])
cIdle =  ("CIdle",[(ElmMaybe (ElmIntRange 0 255,"n",""),"maybeN","")])

newClick = ("Click",[(ElmMaybe (ElmIntRange 0 255,"n",""),"maybeNewN","")])
newNumber = ("ReqNewNumber",[(ElmMaybe (ElmIntRange 0 255,"n",""),"maybeNewN","")])
confNewNumber = ("ConfirmNewNumber",[(ElmMaybe (ElmIntRange 0 255,"n",""),"maybeNewN","")])

clientConnect = ("ClientConnect",[])
clientDisconnect = ("ClientDisconnect",[])

testServer = (
                "CIdle" --client start state
             ,  "Idle" --server start start
             ,  M.fromList [("CIdle",cIdle)] --client states
             ,  M.fromList [("Idle",sIdle)]                                 --server states
             ,  M.fromList [("Colour",testRGB)]                            --extra client types
             ,  M.fromList [("Colour",testRGB)]        --extra server types
             ,  csDiagram --client state diagram
             ,  ssDiagram --server state diagram
             )

csDiagram :: ClientStateDiagram
csDiagram = M.fromList 
            [
                (("CIdle",    newClick)          ,("CIdle",    Just newNumber))
            ,   (("CIdle",    confNewNumber)     ,("CIdle",    Nothing))
            ]

ssDiagram :: ServerStateDiagram
ssDiagram = M.fromList 
            [
                (("Idle", newNumber), ("Idle", ToAll confNewNumber))
            ,   (("Idle", clientConnect), ("Idle", NoClientMessage))
            ,   (("Idle", clientDisconnect), ("Idle", NoClientMessage))
            ]