{-# LANGUAGE QuasiQuotes #-}

module ServerTemplate.Main where

import Text.RawString.QQ
import Data.Text as T

mainHs :: T.Text
mainHs = T.pack $ [r|

module Main where

import           Static.Lib

main :: IO ()
main = mainServer|]