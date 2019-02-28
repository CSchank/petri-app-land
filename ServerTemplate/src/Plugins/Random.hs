module Plugins.Random where

import Static.ServerTypes
import System.Random
import Static.Cmd as Cmd (Cmd(..))


getRandomR :: Random a => (a -> msg) -> (a, a) -> Cmd msg
getRandomR msg (lo,hi) = 
    Cmd (return . msg =<< randomRIO (lo,hi))