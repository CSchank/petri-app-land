module TypeHelpers where

import Types

msg = constructor
request = constructor
response = constructor

constructor :: String -> [DocTypeT] -> Constructor
constructor name edts =
    (name, edts)

edt :: TypeT -> String -> String -> DocTypeT
edt et name desc =
    (et, name, desc)

ec :: String -> [Constructor] -> CustomT
ec name constrs =
    CustomT name constrs

clientID :: DocTypeT
clientID = edt (IntRangeT 0 999999) "clientID" "id assigned when logging in"-- almost a type alias

natural :: TypeT
natural =
    IntRangeT 0 (2^32)