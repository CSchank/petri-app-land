module TestNet.Static.Update exposing(..)
import TestNet.Static.Types
import TestNet.Static.Wrappers
import TestNet.Static.FromSuperPlace exposing (FromSuperPlace(..))
import TestNet.Update as Update

update : TopLevelData -> Transition -> NetState -> (NetState,Maybe (Cmd Transition))
update tld mClientID trans state =
