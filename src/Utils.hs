module Utils where

import System.Process ( spawnCommand, waitForProcess )
import Data.List (intercalate)

--mkCmd :: String -> [String] -> String
mkCmd cmd args = intercalate " " $ [cmd] ++ args

copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory source target = do
    ps <- spawnCommand (mkCmd "cp" ["-r",source,target])
    exitCode <- waitForProcess ps
    return ()