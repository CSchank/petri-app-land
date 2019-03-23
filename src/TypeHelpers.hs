module TypeHelpers where

import Types

msg = constructor
request = constructor
response = constructor

constructor :: String -> [DocTypeT] -> Constructor
constructor name edts =
    (name, edts)

dt :: TypeT -> String -> String -> DocTypeT
dt et name desc =
    (et, name, desc)

ct :: String -> [Constructor] -> CustomT
ct name constrs =
    CustomT name constrs

clientID :: DocTypeT
clientID = dt (IntRangeT 0 999999) "clientID" "id assigned when logging in"-- almost a type alias

natural :: TypeT
natural =
    IntRangeT 0 (2^32)