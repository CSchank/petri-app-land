module Main where

import Types
import qualified Data.Map as M
import Generate.Server 
import Generate.Client 
import TypeHelpers
import ClientServerSpec

import System.Environment

main = do 
    generateServer 
            True              --True: GraphicSVG, False: Elm Html
            False             --True: regenerate only static files, False: regenerate static files and user files if they don't exist
            outputDirectory   --directory
            clientServerApp   --the server to generate
    generateClient 
            True              --True: GraphicSVG, False: Elm Html
            False             --True: regenerate only static files, False: regenerate static files and user files if they don't exist
            outputDirectory   --directory
            clientServerApp   --the server to generate
    