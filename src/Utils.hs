module Utils where

import System.Process ( spawnCommand, waitForProcess )
import Data.List (intercalate)
import                  System.FilePath.Posix


--mkCmd :: String -> [String] -> String
mkCmd cmd args = intercalate " " $ [cmd] ++ args

copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory source target = do
    newCmd <- return $ mkCmd "rsync" ["-r","-u",source,target]
    ps <- spawnCommand newCmd
    exitCode <- waitForProcess ps
    return ()

(|>) = flip ($)