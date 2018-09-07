{-# LANGUAGE OverloadedStrings #-}
module RGBTest where

import Types
import Data.Map as M
import Generate

rgbClient :: ClientStateDiagram
rgbClient = M.fromList 
	[(("Red","Change")		,("Blue",Nothing))
	,(("Red","Click")		,("Red",Just "ChangeReq"))
	,(("Blue","Change")		,("Green",Nothing))
	,(("Blue","Click")		,("Blue",Just "ChangeReq"))
	,(("Green","Change")	,("Red",Nothing))
	,(("Green","Click")		,("Green",Just "ChangeReq"))
	]

rgbServer :: ServerStateDiagram
rgbServer = M.fromList 
	[(("Idle","ChangeReq")	,("Idle",Just "Change"))
	]

rgbApp :: ClientServerApp
rgbApp = ("Red", "Idle", rgbClient, rgbServer)