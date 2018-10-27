module TypeHelpers where

import Types

msg = constructor
state = constructor

constructor :: String -> [ElmDocType] -> Constructor
constructor name edts =
    (name, edts)

edt :: ElmType -> String -> String -> ElmDocType
edt et name desc =
    (et, name, desc)

ec :: String -> [Constructor] -> ElmCustom
ec name constrs =
    ElmCustom name constrs