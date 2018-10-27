{-# LANGUAGE QuasiQuotes #-}

module ClientTemplate.Lib where

import Text.RawString.QQ
import Data.Text as T

libElm :: T.Text
libElm = T.pack $ [r||]