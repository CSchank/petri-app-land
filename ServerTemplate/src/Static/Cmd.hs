module Static.Cmd where

import qualified Plugins.Incrementer
import Static.ServerTypes
import qualified Data.TMap as TM

initStateCmds :: IO PluginState
initStateCmds = do
    let pluginState = TM.empty
    p1 <- initPlugin :: IO Plugins.Incrementer.Counter
    return $ TM.insert p1 pluginState