{-# LANGUAGE OverloadedStrings #-}

module Git where

import Data.Aeson
import Network.HTTP.Simple
import Network.HTTP.Conduit
import Codec.Archive.Zip
import System.Directory
import Control.Monad (when)
import System.Console.ANSI

newtype Zip = Zip String
    deriving (Show)

instance FromJSON Zip where
    parseJSON = withObject "Zip" $ \ v -> Zip <$> v .: "zipball_url"

requestWithUA :: String -> IO Request
requestWithUA url = do
    initReq <- parseRequest url
    return $ setRequestHeader "user-agent" [""] $ setRequestSecure True initReq

loadLatestTemplates :: IO ()
loadLatestTemplates = do
    request <- requestWithUA "https://api.github.com/repos/CSchank/PAL-templates/releases/latest"
    print request
    latestRelease <- httpLBS request
    let mZip = decode $ getResponseBody latestRelease
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