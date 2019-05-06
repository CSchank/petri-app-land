{-# LANGUAGE OverloadedStrings #-}

module Generate.ReduceDotGraph where

import System.IO
import System.Environment
import Data.List

loadDotGraph :: FilePath -> IO String
loadDotGraph fileName = readFile fileName

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates list = remDups list []

remDups :: (Eq a) => [a] -> [a] -> [a]
remDups [] _ = []
remDups (x:xs) list2
    | (x `elem` list2) = remDups xs list2
    | otherwise = x : remDups xs (x:list2)

reduceDotGraph :: FilePath -> IO ()
reduceDotGraph dotGraph = do
    str <- loadDotGraph dotGraph
    let lst = lines str
    let noDups = removeDuplicates lst
    -- Remove self transitions from reduced output
    let noSelfTransitions = [n | n <- noDups, isInfixOf "style=dashed" n == False]
    let strReduced = intercalate "\n" noSelfTransitions
    writeFile (dotGraph ++ "_reduced.dot") strReduced