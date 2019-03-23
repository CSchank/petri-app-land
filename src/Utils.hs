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

getPlaceState :: Place -> Constructor
getPlaceState p =
    case p of
        (Place n _ s _ _ _) -> (T.unpack n,s)

getPlaceName :: Place -> T.Text
getPlaceName p =
    case p of
        (Place n _ _ _ _ _) -> n


getPlayerState :: Place -> Constructor
getPlayerState p =
    case p of
        (Place n _ s _ _ _) -> (T.unpack n,s)
        
getNetName :: Net -> T.Text
getNetName (Net name _ _ _ _) = name

getTransitionName :: Transition -> T.Text
getTransitionName (Transition _ (name,_) _ _) = T.pack name
getTransitionName (ClientTransition (name,_) _ _) = T.pack name
getTransitionName (CmdTransition (name,_) _ _) = T.pack name

findConstrImports :: Language -> Constructor -> [T.Text]
findConstrImports l (_, ets) = 
    concatMap (findImports l) ets

findImports :: Language -> DocTypeT -> [T.Text]
findImports l (PairT edt0 edt1, _, _)          = findImports l edt0 ++ findImports l edt1
findImports l (TripleT edt0 edt1 edt2, _, _)   = findImports l edt0 ++ findImports l edt1 ++ findImports l edt2
findImports l (ListT edt, _, _)                = if l == Haskell then ["import Static.List (List)"] else [] ++ findImports l edt
findImports l (DictT edt0 edt1, _, _)          = 
    if l == Haskell then ["import Static.Dict (Dict)"] else ["import Dict exposing (Dict)"] 
        ++ findImports l edt0 ++ findImports l edt1
findImports l (ExistingT name imp, _, _)       = 
    if l == Haskell then [T.concat ["import qualified ",T.pack imp, " (",T.pack name,")"]]
    else [T.concat ["import ",T.pack imp]]
findImports l (ExistingWParamsT name params imp, _, _)       = 
    if l == Haskell then ([T.concat ["import qualified ",T.pack imp, " (",T.pack name,")"]] ++ map (\(param,mod) -> T.concat ["import qualified ",T.pack mod, " (",T.pack param,")"]) params)
    else ([T.concat ["import ",T.pack imp]] ++ map (\(param,mod) -> T.concat ["import ",T.pack mod]) params)
findImports l (ResultT edt0 edt1, _, _)       = 
    (if l == Haskell then ["import Static.Result (Result(..))"] else [])
        ++ findImports l edt0 ++ findImports l edt1
findImports _ _ = []