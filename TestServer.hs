module TestServer where

import Types
import qualified Data.Map as M
import Generate.Server 

red = ("Red",[])
blue = ("Blue",[])
green = ("Green",[])

idle = ("Idle",[])

click = ("Click",[])
change = ("Change",[])

testServer = (
				"Red"
			 ,  "Idle"
			 ,  [red,blue,green]
			 ,  [idle]
			 ,  csDiagram
			 ,  ssDiagram
			 )

csDiagram :: ClientStateDiagram
csDiagram = M.fromList 
			[
				(("Red",	click) 	,("Red",	Just click))
			,	(("Red",	change)	,("Red",	Nothing))
			,   (("Green",	click) 	,("Green",	Just click))
			,	(("Green",	change)	,("Blue",	Nothing))
			,   (("Blue",	click) 	,("Blue",	Just click))
			,	(("Blue",	change)	,("Red",	Nothing))
			]

ssDiagram :: ServerStateDiagram
ssDiagram = M.fromList 
			[
				(("Idle", click), ("Idle", Just change))
			]