module TestServer where

import Types
import qualified Data.Map as M
import Generate.Server 
import TestElmTypes

red = ("Red",[])
blue = ("Blue",[])
green = ("Green",[])

idle = ("Idle",[(ElmIntRange 0 100000000,"num","The number of clicks that users have made.")])

click = ("Click",[])
reqNew = ("RequestNewColour",[])
change = ("ChangeColour",[(ElmType "Colour","colour","")])

testServer = (
                "Red" --client start state
             ,  "Idle" --server start start
             ,  M.fromList [("Red",red),("Blue",blue),("Green",green)] --client states
             ,  M.fromList [("Idle",idle)]                                 --server states
             ,  M.fromList [("Colour",testRGB)]                            --extra client types
             ,  M.fromList [("Colour",testRGB)]        --extra server types
             ,  csDiagram --client state diagram
             ,  ssDiagram --server state diagram
             )

csDiagram :: ClientStateDiagram
csDiagram = M.fromList 
            [
                (("Red",    click)     ,("Red",    Just reqNew))
            ,    (("Red",    change)    ,("Red",    Nothing))
            ,   (("Green",    click)     ,("Green",    Just reqNew))
            ,    (("Green",    change)    ,("Blue",    Nothing))
            ,   (("Blue",    click)     ,("Blue",    Just reqNew))
            ,    (("Blue",    change)    ,("Red",    Nothing))
            ]

ssDiagram :: ServerStateDiagram
ssDiagram = M.fromList 
            [
                (("Idle", reqNew), ("Idle", Just change))
            ]