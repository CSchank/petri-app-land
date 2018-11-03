module Utils where

import System.File.Tree (getDirectory, copyTo_)

copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory source target = getDirectory source >>= copyTo_ target