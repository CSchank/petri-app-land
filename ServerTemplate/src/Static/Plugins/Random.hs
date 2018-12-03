module Static.Plugins.Random where

import Static.ServerTypes
import System.Random

getRandomR :: Random a => (a -> msg) -> (a, a) -> Cmd msg
getRandomR msg (lo,hi) = 
    Cmd (return . msg =<< randomRIO (lo,hi))