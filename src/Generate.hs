module Generate where

import Validate
import System.Console.ANSI
import Types
import Generate.Client
import Generate.Server
import Control.Monad (unless,when)
import System.Directory (doesDirectoryExist)
import System.FilePath.Posix ((</>))
import System.Exit (exitFailure)
import Generate.Dot

generate :: FilePath -> FilePath -> ClientServerApp -> IO ()
generate outputDirectory rootDir clientServerApp = do
    let errors = validateCSApp clientServerApp
    let templateDir = (rootDir </> "ClientTemplate/")
    exists <- doesDirectoryExist templateDir
    unless exists $ do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn $ "Error: cannot find client template directory at `" ++ templateDir ++ "`. Please update rootDir to the proper directory."
        setSGR [SetColor Foreground Vivid White]
        exitFailure
    let templateDir = (rootDir </> "ServerTemplate/")
    unless exists $ do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn $ "Error: cannot find server template directory at `" ++ templateDir ++ "`. Please update rootDir to the proper directory."
        setSGR [SetColor Foreground Vivid White]
        exitFailure

    when (length errors > 0) $ do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn $ show (length errors) ++ " error"++if length errors > 1 then "s" else ""++" detected in spec:"
        setSGR [SetColor Foreground Vivid White]
        mapM_ putStrLn $ map (\(n,e) -> "["++show n ++"]: " ++ e) $ zip [1..] errors
        exitFailure
    
    setSGR [SetColor Foreground Vivid Green]
    putStrLn "No errors detected in spec!"
    setSGR [SetColor Foreground Vivid White]
    generateServer 
            True              --True: GraphicSVG, False: Elm Html
            rootDir             --True: regenerate only static files, False: regenerate static files and user files if they don't exist
            outputDirectory   --directory
            clientServerApp   --the server to generate
    generateClient 
            True              --True: GraphicSVG, False: Elm Html
            rootDir             --True: regenerate only static files, False: regenerate static files and user files if they don't exist
            outputDirectory   --directory
            clientServerApp   --the server to generate
    generateDot clientServerApp outputDirectory False