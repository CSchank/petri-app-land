module TypeHelpers where

import Types

msg = constructor
request = constructor
response = constructor

constructor :: String -> [ElmDocType] -> Constructor
constructor name edts =
    (name, edts)

edt :: ElmType -> String -> String -> ElmDocType
edt et name desc =
    (et, name, desc)

ec :: String -> [Constructor] -> ElmCustom
ec name constrs =
    ElmCustom name constrs

clientID :: ElmDocType
clientID = edt (ElmIntRange 0 999999) "clientID" "id assigned when logging in"-- almost a type alias

natural :: ElmType
natural =
    ElmIntRange 0 (2^32)