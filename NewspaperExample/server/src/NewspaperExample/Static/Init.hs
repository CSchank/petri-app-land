module NewspaperExample.Static.Init where
import NewspaperExample.Static.Types (Player)
import NewspaperExample.Init as Init
import NewspaperExample.Update as Update
import NewspaperExample.Static.Wrappers
import NewspaperExample.Static.Plugins (initPlugins)
import Static.ServerTypes
import qualified Data.IntMap as IM'
import Data.Maybe (fromJust)
import qualified Data.TMap as TM

init :: IO (NetState Player)
init = do
    ip <- initPlugins
    return $ NetState
        {
          playerStates = IM'.empty
        , placeStates = TM.insert initMainStreet $ TM.insert initReadingRoom $ TM.insert initEditingRoom $ TM.empty
        , pluginStates = ip
        }
