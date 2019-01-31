{-# LANGUAGE OverloadedStrings #-}

module Generate.Plugins where

import qualified Data.Text as T
import Types
import                  System.FilePath.Posix   ((</>),(<.>))
import Utils


generatePlugins :: FilePath -> T.Text -> [Plugin] -> IO ()
generatePlugins fp netName ps = 
    let
        onePlugin n (Plugin name) = T.concat["    (_,rp",T.pack $ show n,") <- forkIO $ (initPlugin :: IO Plugins.",T.pack name,".",T.pack name,")"]
        onePlugin n (PluginGen name _) = T.concat["    (_,rp",T.pack $ show n,") <- forkIO $ (initPlugin :: IO Plugins.",T.pack name,".",T.pack name,")"]
        oneResult n               = T.concat["    p",T.pack $ show n," <- result =<< rp", T.pack $ show n]
        ret n                     = T.concat["    return $ ",T.concat $ map (\n -> T.concat ["TM.insert p",T.pack $ show n," $ "]) [0..length ps - 1], "TM.empty"]
    in do
        writeIfNew 0 (fp </> "Static" </> "Plugins" <.> "hs") $ T.unlines
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
        mapM_ (\p -> 
                    case p of 
                        Plugin _ -> return ()
                        PluginGen name gen -> do
                            g <- gen
                            mapM_ (\(n,t) -> 
                                    writeIfNew 0 (if n == "" then fp </> "Static" </> "Plugins" </> name <.> "hs"
                                                  else fp </> "Static" </> "Plugins" </> name </> n <.> "hs") t) g
            ) ps -- generate other files for plugins