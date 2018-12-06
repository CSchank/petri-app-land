{-# LANGUAGE OverloadedStrings #-}

module Generate.Plugins where

import qualified Data.Text as T
import Types

generatePlugins :: [Plugin] -> T.Text
generatePlugins ps = 
    let
        onePlugin n (Plugin name) = T.concat["    (_,rp",T.pack $ show n,") <- forkIO $ (initPlugin :: IO Plugins.",T.pack name,".",T.pack name,")"]
        oneResult n               = T.concat["    p",T.pack $ show n," <- result =<< rp", T.pack $ show n]
        return n                  = T.concat["    return $ ",T.concat $ map (\n -> T.concat ["TM.insert p",T.pack $ show n," $ "]) [0..length ps - 1], "TM.empty"]
    in
        T.unlines $
            [
                "module Static.Plugins where"
            ,   "import Static.ServerTypes"
            ,   "import qualified Data.TMap as TM"
            ,   "import Control.Concurrent.Thread (forkIO, result)\n"
            ,   T.concat $ map (\(Plugin p) -> T.concat["import qualified Plugins.",T.pack p,"\n"]) ps,""
            ,   "initStateCmds :: IO PluginState"
            ,   "initStateCmds = do"
            ] 
            ++
            (map (uncurry onePlugin) $ zip [0..] ps)
            ++
            (map oneResult [0..length ps - 1])
            ++
            [
                return $ length ps
            ]