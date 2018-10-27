{-# LANGUAGE QuasiQuotes #-}

module ClientTemplate.Version where

import Text.RawString.QQ
import Data.Text as T

versionElm :: T.Text
versionElm = T.pack $ [r|module Static.Version exposing(version)

version : String
version = "v0.1"
|]