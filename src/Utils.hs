{-# LANGUAGE OverloadedStrings #-}

module Utils where

import System.Process ( spawnCommand, waitForProcess )
import Data.List (intercalate, nub)
import                  System.FilePath.Posix
import                  Data.Maybe              (mapMaybe,fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import                  System.Directory
import                  Control.Monad (unless)
import Data.Char (toUpper,toLower)
import qualified Data.Set as S
import Types



--mkCmd :: String -> [String] -> String
mkCmd cmd args = intercalate " " $ [cmd] ++ args

copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory source target = do
    newCmd <- return $ mkCmd "rsync" ["-r","-u",source,target]
    ps <- spawnCommand newCmd
    exitCode <- waitForProcess ps
    return ()

(|>) = flip ($)

--https://stackoverflow.com/questions/21349408/zip-with-default-value-instead-of-dropping-values
zipWithDefault :: (Eq t0, Eq t1) => t0 -> t1 -> [t0] -> [t1] -> [(t0,t1)]
zipWithDefault dx dy xl yl = 
  map (\(x,y) -> (fromMaybe dx x, fromMaybe dy y)) $ 
    takeWhile (/= (Nothing, Nothing)) $ 
    zip ((map Just xl) ++ (repeat Nothing)) ((map Just yl) ++ (repeat Nothing))

writeIfNotExists :: FilePath -> T.Text -> IO ()
writeIfNotExists fp txt = do
    exists <- doesFileExist fp
    Prelude.putStrLn $ fp ++ " exists:" ++ show exists
    unless exists $ TIO.writeFile fp txt

-- write the file if more than one line (the date line) has changed
writeIfNew :: Int -> FilePath -> T.Text -> IO ()
writeIfNew n fp txt = do
    exists <- doesFileExist fp
    if not exists then do
            Prelude.putStrLn $ fp ++ " exists:" ++ show exists
            unless exists $ TIO.writeFile fp txt
         else do
            currentLines <- return . T.lines =<< TIO.readFile fp
            let diffLines = filter (\(a,b) -> a /= b) $ zipWithDefault "" "" currentLines (T.lines txt)
            Prelude.putStr $ "Differences in " ++ fp ++ " : " ++ show (length diffLines)
            if length diffLines > n then do
                putStr " [rewriting]\n"
                TIO.writeFile fp txt
            else do
                putStr " [skipping]\n"
                return ()

disclaimer date = T.unlines ["{-"
                        ,T.concat["    THIS FILE WAS AUTOMATICALLY GENERATED AT ", T.pack $ show date,"."]
                        , "    IMPORTANT: USE THIS FILE FOR REFERENCE ONLY. YOU SHOULD NOT MODIFY THIS FILE. INSTEAD, MODIFY THE STATE DIAGRAM AND REGENERATE THIS FILE."
                        , "    MODIFYING ANY FILES INSIDE THE Static DIRECTORY COULD LEAD TO UNEXPECTED ERRORS IN YOUR APP."
                        ,"-}"
                        ]

capitalize :: T.Text -> T.Text
capitalize txt =
    case T.unpack txt of
        h:rest -> T.pack $ toUpper h : rest
        _ -> txt
    

uncapitalize :: T.Text -> T.Text
uncapitalize txt =
    case T.unpack txt of
        h:rest -> T.pack $ toLower h : rest
        _ -> txt

capStr :: String -> String
capStr str =
    case str of
        h:rest -> toUpper h : rest
        _ -> str

fnub :: Ord a => [a] -> [a]
fnub = nub --S.toList . S.fromList

getPlaceState :: HybridPlace -> Constructor
getPlaceState p =
    case p of
        (HybridPlace n _ s _ _ _ _) -> (T.unpack n,s)

getPlaceName :: HybridPlace -> T.Text
getPlaceName p =
    case p of
        (HybridPlace n _ _ _ _ _ _) -> n


getPlayerState :: HybridPlace -> Constructor
getPlayerState p =
    case p of
        (HybridPlace n _ s _ _ _ _) -> (T.unpack n,s)
        
getNetName :: Net -> T.Text
getNetName (HybridNet name _ _ _ _) = name

getTransitionName :: NetTransition -> T.Text
getTransitionName (NetTransition (name,_) _ _) = T.pack name