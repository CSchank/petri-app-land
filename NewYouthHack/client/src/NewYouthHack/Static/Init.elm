module NewYouthHack.Static.Init exposing(..)
import NewYouthHack.Init as Init
import NewYouthHack.Static.Types exposing (NetState(..))
import NewYouthHack.Update as Update
import NewYouthHack.Static.Wrappers
init : NetState
init = SSquareOne Init.init
