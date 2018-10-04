module TestServer where

import Types
import qualified Data.Map as M
import Generate.Server 
import TestElmTypes

sIdle = ("Idle",[(ElmType "Colour","colour","The current colour on the server.")])
cIdle = ("CIdle",[(ElmType "Colour","colour","The current colour on the client.")])

click = ("Click",[])
reqNew = ("NewColour",[(ElmType "Colour","newColour","The colour that the client is requesting to change to.")])
update = ("UpdateColour",[(ElmType "Colour","updateColour","The colour that the server is updating the clients with.")])

incRed = ("IncRed",[])
incBlue = ("IncBlue",[])
incGreen = ("IncGreen",[])

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
                (("CIdle",    incRed)     ,("CIdle",    Just reqNew))
            ,   (("CIdle",    incBlue)     ,("CIdle",    Just reqNew))
            ,   (("CIdle",    incGreen)     ,("CIdle",    Just reqNew))
            ,   (("CIdle",    update)       ,("CIdle",    Nothing))
            ]

ssDiagram :: ServerStateDiagram
ssDiagram = M.fromList 
            [
                (("Idle", reqNew), ("Idle", ToAll update))
            ]