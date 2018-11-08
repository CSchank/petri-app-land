module TypeHelpers where

import Types

msg = constructor

cState :: String -> [ElmDocType] -> ClientState
cState n edts = ClientState $ constructor n edts

withSub :: Constructor -> ClientState -> ClientState
withSub sub (ClientState constr)                            = ClientStateWithSubs constr [sub]
withSub sub (ClientStateWithSubs constr subs)               = ClientStateWithSubs constr (sub:subs)

sState :: String -> [ElmDocType] -> Constructor
sState = constructor


constructor :: String -> [ElmDocType] -> Constructor
constructor name edts =
    (name, edts)

edt :: ElmType -> String -> String -> ElmDocType
edt et name desc =
    (et, name, desc)

ec :: String -> [Constructor] -> ElmCustom
ec name constrs =
    ElmCustom name constrs