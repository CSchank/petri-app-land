module RGBTest where

import Types
import Data.Map as M
import Generate

rgbClient :: ClientStateDiagram
rgbClient = M.fromList 
	[((PlainType "Red",PlainType "Ch"),(PlainType "Blue",Nothing))
	,((PlainType "Red",PlainType "Click"),(PlainType "Red",Just $ PlainType "Click"))
	,((PlainType "Blue",PlainType "Ch"),(PlainType "Green",Nothing))
	,((PlainType "Blue",PlainType "Click"),(PlainType "Blue",Just $ PlainType "Click"))
	,((PlainType "Green",PlainType "Ch"),(PlainType "Red",Nothing))
	,((PlainType "Green",PlainType "Click"),(PlainType "Green",Just $ PlainType "Click"))
	]

rgbServer :: ServerStateDiagram
rgbServer = M.fromList 
	[((PlainType "Idle",PlainType "Click"),(PlainType "Idle",Just $ PlainType "Ch"))
	]

rgbApp :: ClientServerApp
rgbApp = (rgbClient, rgbServer)