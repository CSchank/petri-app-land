{-# LANGUAGE OverloadedStrings #-}

module Generate.Version where

import qualified Data.Time.Clock.POSIX         as Time
import Data.Text as T

generateVersion :: IO T.Text
generateVersion = do
    t <- Time.getPOSIXTime
    return $ T.concat ["v", T.pack $ show (t * 10^6)]