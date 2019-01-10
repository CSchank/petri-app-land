module Static.Plugins where

import qualified Plugins.Incrementer
import Static.ServerTypes
import qualified Data.TMap as TM
import Control.Concurrent.Thread (forkIO)

initStateCmds :: IO PluginState
initStateCmds = do
    let pluginState = TM.empty
    (_,p1) <- forkIO $ initPlugin :: IO Plugins.Incrementer.Counter
    return $ TM.insert p1 pluginState