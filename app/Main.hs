module Main where

import Types
import qualified Data.Map as M
import Generate.Server 
import TypeHelpers
import ClientServerSpec

import System.Environment

main = generateServer 
            True                        --True: GraphicSVG, False: Elm Html
            False                       --True: regenerate only static files, False: regenerate static files and user files if they don't exist
            "../elm-fraction-drawer/"   --directory
            clientServerApp             --the server to generate