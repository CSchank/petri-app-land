{-# LANGUAGE OverloadedStrings #-}

module Generate.Plugins where

import qualified Data.Text as T
import Types
import                  System.FilePath.Posix   ((</>),(<.>))
import Utils
import qualified Data.Map.Strict as M'


generatePlugins :: FilePath -> M'.Map String CustomT -> Net -> [Plugin] -> IO ()
generatePlugins fp extraTypes net ps = 
    let
        netName = getNetName net

        onePlugin n (Plugin name) = T.concat["    (_,rp",T.pack $ show n,") <- forkIO $ (initPlugin :: IO Plugins.",T.pack name,".",T.pack name,")"]
        onePlugin n (PluginGen name _) = T.concat["    (_,rp",T.pack $ show n,") <- forkIO $ (initPlugin :: IO Plugins.",T.pack name,".",T.pack name,")"]
        oneResult n               = T.concat["    p",T.pack $ show n," <- result =<< rp", T.pack $ show n]
        ret n                     = T.concat["    return $ ",T.concat $ map (\n -> T.concat ["TM.insert p",T.pack $ show n," $ "]) [0..length ps - 1], "TM.empty"]

        pluginTD n (Plugin name) = T.concat["    (_,rp",T.pack $ show n,") <- forkIO $ teardownPlugin (fromJust $ TM.lookup ps :: Plugins.",T.pack name,".",T.pack name,")"]
        pluginTD n (PluginGen name _) = T.concat["    (_,rp",T.pack $ show n,") <- forkIO $ teardownPlugin (fromJust $ TM.lookup ps :: Plugins.",T.pack name,".",T.pack name,")"]

    in do
        writeIfNew 0 (fp </> T.unpack netName </> "Static" </> "Plugins" <.> "hs") $ T.unlines $
            ([
                T.concat["module ",netName,".Static.Plugins where"]
            ,   T.concat["import ",netName,".Static.Types"]
            ,   "import Static.ServerTypes"
            ,   "import qualified Data.TMap as TM"
            ,   "import Control.Concurrent.STM (TQueue, atomically, writeTQueue)"
            ,   "import           Control.Monad          (void)"
            ,   "import Data.Maybe (fromJust)"
            ,   "import Control.Concurrent.Thread (forkIO, result)\n"
            ,   T.concat $ map (\p -> case p of 
                                    Plugin n -> T.concat["import qualified Plugins.",T.pack n,"\n"]
                                    PluginGen n _ -> T.concat["import qualified Plugins.",T.pack n,"\n"]
                                ) ps,""
            ,   ""
            ,   "initPlugins :: IO PluginState"
            ,   "initPlugins = do"
            ] 
            ++
            (map (uncurry onePlugin) $ zip [0..] ps)
            ++
            (map oneResult [0..length ps - 1])
            ++
            [ ret $ length ps ])
            ++
            [
                "teardownPlugins :: PluginState -> IO ()"
            ,   "teardownPlugins ps = do"
            ,   T.intercalate "\n" $ map (uncurry pluginTD) $ zip [0..] ps
            ,   T.intercalate "\n" $ map oneResult [0..length ps - 1]
            ,   "    return ()"
            ]
        mapM_ 
            (\p -> 
                case p of 
                    Plugin _ -> return ()
                    PluginGen name gen -> do
                        g <- gen extraTypes net
                        mapM_ (\(n,t) -> 
                                writeIfNew 0 (if n == "" then fp </> "Plugins" </> name <.> "hs"
                                                else fp </> "Plugins" </> name </> n <.> "hs") t) g
            ) ps -- generate other files for plugins