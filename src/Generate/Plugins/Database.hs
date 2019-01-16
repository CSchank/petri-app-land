{-# LANGUAGE OverloadedStrings #-}

module Generate.Plugins.Database where

import Types
import TypeHelpers
import Generate.Types
import qualified Data.Text as T
import Utils

type Key = ElmDocType
type Data = ElmDocType

data Table = 
    Table String {-the name of the table-} [Key] {-the keys in the row-} [Data] {-the data in the row-}

generateDatabase :: [Table] -> IO [(FilePath, T.Text)]
generateDatabase ts =
    let
        generateOneTable (Table name keys dat) =
            let
                rowT = ec name $ [constructor name (keys++dat)]
                safecopy n = T.concat ["$(deriveSafeCopy 0 'base ", "''",T.pack $ n,")"]
            in T.unlines 
                [
                    "{-# LANGUAGE TemplateHaskell #-}"
                ,   T.concat["module Plugins.Database.Table.",T.pack name," where"]
                ,   "import Data.Data            (Data, Typeable)"
                ,   "import Data.IxSet           (Indexable(..), IxSet(..), (@=), Proxy(..), getOne, ixFun, ixSet)"
                ,   "import Data.SafeCopy        (SafeCopy, base, deriveSafeCopy)"
                ,   "import Static.Types"
                ,   "import Plugins.Database.Query",""
                ,   generateType Haskell True [DOrd,DEq,DShow,DData,DTypeable] rowT,""
                ,   generateNewtype True [DOrd,DEq,DShow,DData,DTypeable] (name++"Record") (ElmPair (ElmType "Index", "index", "") (ElmType name, name, ""))
                ,   safecopy (name++"Record"),""
                ,   T.unlines $ map (\(et,n,_) -> generateNewtype True [DOrd,DEq,DShow,DData,DTypeable] (capStr n) et) keys
                ,   T.unlines $ map (\(_,n,_) -> safecopy n) keys
                ]
        main = T.unlines 
                [
                    "module Plugins.Database where"
                ,   T.unlines $ map (\(Table name _ _) -> T.concat ["import Plugins.Database.Table.",T.pack name]) ts
                ,   ""
                ]
    in
        return $ [("", main)] ++
                 map (\(table@(Table name _ _)) -> ("Table/"++name,generateOneTable table)) ts

