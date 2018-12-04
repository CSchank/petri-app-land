module Static.Plugins.Time where

import Static.ServerTypes
import qualified Data.Time.Clock.POSIX         as Time


getPosixTime :: (Int -> msg) -> Cmd msg
getPosixTime msg =
    Cmd (return . msg . round =<< Time.getPOSIXTime)