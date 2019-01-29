module Main where

import Types
import qualified Data.Map as M
import Generate.Server 
import Generate.Client 
import TypeHelpers
import ClientServerSpec
import Validate

import System.Environment
import System.Console.ANSI

main = do
    let errors = validateCSApp clientServerApp
    if length errors > 0 then do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn $ show (length errors) ++ " error"++if length errors > 1 then "s" else ""++" detected in spec:"
        setSGR [SetColor Foreground Vivid White]
        mapM_ putStrLn $ map (\(n,e) -> "["++show n ++"]: " ++ e) $ zip [1..] errors
    else do
        setSGR [SetColor Foreground Vivid Green]
        putStrLn "No errors detected in spec!"
        setSGR [SetColor Foreground Vivid White]
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
    