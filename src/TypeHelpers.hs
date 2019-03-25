{-# LANGUAGE OverloadedStrings #-}

module TypeHelpers where

import Types
import qualified Data.Text as T

msg = constructor
request = constructor
response = constructor

constr = constructor

constructor :: T.Text -> [DocTypeT] -> Constructor
constructor name edts =
    (name, edts)

dt :: TypeT -> T.Text -> T.Text -> DocTypeT
dt et name desc =
    (et, name, desc)

ct :: T.Text -> [Constructor] -> CustomT
ct name constrs =
    CustomT name constrs

clientID :: DocTypeT
clientID = dt (IntRangeT 0 999999) "clientID" "id assigned when logging in"-- almost a type alias

natural :: TypeT
natural =
    IntRangeT 0 (2^32)