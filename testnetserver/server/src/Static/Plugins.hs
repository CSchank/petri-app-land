module Static.Plugins where
import Static.ServerTypes
import qualified Data.TMap as TM
import Control.Concurrent.Thread (forkIO, result)



initStateCmds :: IO PluginState
initStateCmds = do
    return $ TM.empty
