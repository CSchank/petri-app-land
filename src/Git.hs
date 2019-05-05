{-# LANGUAGE OverloadedStrings #-}

module Git where

import Data.Aeson
import Network.HTTP.Simple
import Network.HTTP.Conduit
import Codec.Archive.Zip
import System.Directory

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
    latestRelease <- httpLBS request
    let mZip = decode $ getResponseBody latestRelease
    case mZip of
        Just (Zip url) -> do
            zipReq <- requestWithUA "https://github.com/CSchank/PAL-templates/archive/0.0.1.zip"
            zipBody <- httpLBS zipReq
            let zip = toArchive $ getResponseBody zipBody
            let root = head $ filesInArchive zip
            extractFilesFromArchive [OptDestination "."] zip
            renamePath root ".templates"
        Nothing ->
            error "could not decode latest release from GitHub"