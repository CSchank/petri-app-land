{-# LANGUAGE OverloadedStrings #-}

module Generate.Codec where

import Types (ElmDocType, ElmType, ElmCustom, ClientStateDiagram, ServerStateDiagram, ClientServerApp)
import Data.Map as M

generateCodec :: ClientServerApp -> 