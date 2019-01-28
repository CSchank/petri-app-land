module NewYouthHack.Static.Plugins where
import NewYouthHack.Static.Types
import Static.ServerTypes
import qualified Data.TMap as TM
import Control.Concurrent.STM (TQueue, atomically, writeTQueue)
import           Control.Monad          (void)
import Data.Maybe (fromJust)
import Control.Concurrent.Thread (forkIO, result)




initPlugins :: IO PluginState
initPlugins = do
    return $ TM.empty
