{-# LANGUAGE OverloadedStrings #-}

module Git where

import Data.Aeson
import Network.HTTP.Simple
import Network.HTTP.Conduit
import Codec.Archive.Zip
import System.Directory
import Control.Monad (when)
import System.Console.ANSI
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.List

newtype Zip = Zip String
    deriving (Show)

instance FromJSON Zip where
    parseJSON = withObject "Zip" $ \ v -> Zip <$> v .: "zipball_url"

newtype Release = Release String
    deriving (Show)

instance FromJSON Release where
    parseJSON = withObject "Release" $ \ v -> Release <$> v .: "tag_name"

requestWithUA :: String -> IO Request
requestWithUA url = do
    initReq <- parseRequest url
    return $ setRequestHeader "user-agent" [""] $ setRequestSecure True initReq

--getLatestRelease :: String -> IO Bytestring
getLatestRelease repo = do
    request <- requestWithUA $ "https://api.github.com/repos/" ++ repo ++ "/releases/latest"
    rel <- httpLBS request
    return $ getResponseBody rel

getRelease rel repo = do
    request <- requestWithUA $ "https://api.github.com/repos/" ++ repo ++ "/releases/" ++ rel
    rel <- httpLBS request
    return $ getResponseBody rel

loadTemplates :: String -> IO ()
loadTemplates version = do
    latestRelease <- getRelease version "CSchank/PAL-templates"
    let mZip = decode latestRelease
    case mZip of
        Just (Zip url) -> do
            zipReq <- requestWithUA url
            zipBody <- httpLBS zipReq
            let zip = toArchive $ getResponseBody zipBody
            let root = head $ filesInArchive zip
            extractFilesFromArchive [OptDestination "."] zip
            exists <- doesDirectoryExist ".templates"
            when exists $ removeDirectoryRecursive ".templates"
            renamePath root ".templates"
        Nothing -> do
            setSGR [SetColor Foreground Vivid Red]
            putStrLn "Error: Could not decode latest release from GitHub. You may have exceeded the API limit."
            putStrLn "Trying to fall back on last template downloaded..."
            setSGR [Reset]

-- from https://stackoverflow.com/a/5852820
replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs

updatePAL :: IO ()
updatePAL = do
    latestRelease <- getLatestRelease "CSchank/petri-app-land"
    let mRel = decode latestRelease
    case mRel of
        Just (Release rel) -> do
            currentVersion <- T.unpack <$> TIO.readFile ".palversion"
            if currentVersion == rel then do
                putStrLn $ "PAL is already on the latest version (" ++ rel ++ ")"
            else do
                putStrLn $ "You are using an older version of PAL (" ++ currentVersion ++ ")."
                putStrLn $ "The newest version is " ++ rel ++ "."
                putStrLn $ "See changelog at https://github.com/CSchank/petri-app-land/releases/tag/" ++ rel ++ "."
                putStrLn $ "Update to version " ++ rel ++ "? (Y/N)"
                resp <- getLine
                if resp == "y" || resp == "Y" then do 
                    stackYaml <- T.lines <$> TIO.readFile "stack.yaml"
                    case "#PALCOMMIT" `elemIndex` stackYaml of
                        Just line -> do
                            let newYaml = replaceNth (line+1) (T.concat["  commit: ",T.pack rel]) stackYaml
                            TIO.writeFile "stack.yaml" $ T.unlines newYaml
                            putStrLn $ "stack.yaml file updated to reflect new version of PAL"
                            TIO.writeFile ".palversion" $ T.pack rel
                            loadTemplates rel -- download the PAL templates for this version
                            putStrLn $ "Update complete. Version is now " ++ rel ++ "."
                            putStrLn "Run `stack build` again to rebuild your project with the newest version of PAL."
                        Nothing -> 
                            return ()
                else putStrLn "Update aborted. Run `stack exec pal-update` again to update."
        Nothing -> do
            setSGR [SetColor Foreground Vivid Red]
            putStrLn "Error: Could not decode latest release from GitHub. You may have exceeded the API limit."
            setSGR [Reset]
