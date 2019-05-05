{-# LANGUAGE OverloadedStrings #-}

module Generate where

import Validate
import System.Console.ANSI
import Types
import Generate.Client
import Generate.Server
import Generate.Version
import Control.Monad (unless,when)
import System.Directory (doesDirectoryExist)
import System.FilePath.Posix ((</>),(<.>))
import System.Exit (exitFailure)
import Generate.Dot
import qualified Data.Text as T
import Utils
import Git (loadLatestTemplates)

import qualified Data.Text.IO as TIO

generate :: FilePath -> FilePath -> ClientServerApp -> IO ()
generate outputDirectory rootDir clientServerApp = 
    let
        serverVersion vers =
            T.unlines 
            [
                "{-# LANGUAGE OverloadedStrings #-}"
            ,   "module Static.Version where\n"
            ,   "import Data.Text as T"
            ,   "version :: T.Text"
            ,   T.concat["version = \"",vers,"\""]
            ]

        clientVersion vers =
            T.unlines 
            [
                "module Static.Version exposing(version)\n"
            ,   T.concat["version = \"",vers,"\""]
            ]
    in
    do
    putStrLn "Downloading PAL templates......"
    loadLatestTemplates
    let errors = validateCSApp clientServerApp
    let templateDir = (rootDir </> "ClientTemplate/")
    exists <- doesDirectoryExist templateDir
    unless exists $ do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn $ "Error: cannot find client template directory at `" ++ templateDir ++ "`. Please update rootDir to the proper directory."
        setSGR [Reset]
        exitFailure
    let templateDir = (rootDir </> "ServerTemplate/")
    unless exists $ do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn $ "Error: cannot find server template directory at `" ++ templateDir ++ "`. Please update rootDir to the proper directory."
        setSGR [Reset]
        exitFailure

    when (length errors > 0) $ do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn $ show (length errors) ++ " error"++if length errors > 1 then "s" else ""++" detected in spec:"
        setSGR [Reset]
        mapM_ putStrLn $ map (\(n,e) -> "["++show n ++"]: " ++ e) $ zip [1..] errors
        exitFailure
    
    setSGR [SetColor Foreground Vivid Green]
    putStrLn "No errors detected in spec!"
    setSGR [Reset]
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
    vers <- generateVersion
    putStrLn "Writing versions"
    TIO.writeFile (outputDirectory </> "server" </> "src" </> "Static" </> "Version" <.> "hs") $ serverVersion vers
    TIO.writeFile (outputDirectory </> "client" </> "src" </> "Static" </> "Version" <.> "elm") $ clientVersion vers

    generateDot clientServerApp outputDirectory False