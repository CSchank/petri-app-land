module Plugins.Time where

import Static.ServerTypes
import qualified Data.Time.Clock.POSIX         as Time
import Static.Cmd as Cmd (Cmd(..))


getPosixTime :: (Int -> msg) -> Cmd msg
getPosixTime msg =
    Cmd (return . msg . round =<< Time.getPOSIXTime)