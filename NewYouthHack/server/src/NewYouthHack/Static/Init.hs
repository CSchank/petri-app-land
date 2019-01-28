module NewYouthHack.Static.Init where
import NewYouthHack.Static.Types (Player)
import NewYouthHack.Init as Init
import NewYouthHack.Update as Update
import NewYouthHack.Static.Wrappers
import NewYouthHack.Static.Plugins (initPlugins)
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
        , placeStates = TM.insert initSquareOne $ TM.insert initUniversitiesAndColleges $ TM.insert initMcMasterUniversity $ TM.insert initMcMasterCreateNotice $ TM.empty
        , pluginStates = ip
        }
