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
receive = ("Receive",[])
change = ("ChangeColour",[(ElmType "Colour","colour","")])

testServer = (
                "Red"
             ,  "Idle"
             ,  M.fromList [("Red",red),("Blue",blue),("Green",green)]
             ,  M.fromList [("Idle",idle)]
             ,  M.empty
             ,  M.fromList [("Colour",testRGB)]
             ,  csDiagram
             ,  ssDiagram
             )

csDiagram :: ClientStateDiagram
csDiagram = M.fromList 
            [
                (("Red",    click)     ,("Red",    Just receive))
            ,    (("Red",    change)    ,("Red",    Nothing))
            ,   (("Green",    click)     ,("Green",    Just receive))
            ,    (("Green",    change)    ,("Blue",    Nothing))
            ,   (("Blue",    click)     ,("Blue",    Just receive))
            ,    (("Blue",    change)    ,("Red",    Nothing))
            ]

ssDiagram :: ServerStateDiagram
ssDiagram = M.fromList 
            [
                (("Idle", receive), ("Idle", Just change))
            ]