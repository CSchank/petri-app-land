module NewspaperExample.Static.Init exposing(..)
import NewspaperExample.Init as Init
import NewspaperExample.Static.Types exposing (NetState(..))
import NewspaperExample.Update as Update
import NewspaperExample.Static.Wrappers
init : NetState
init = SMainStreet Init.init
