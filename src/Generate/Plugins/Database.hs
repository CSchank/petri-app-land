{-# LANGUAGE OverloadedStrings #-}

module Generate.Plugins.Database where

import Types
import TypeHelpers
import Generate.Types
import qualified Data.Text as T
import Utils
import qualified Data.Map.Strict as M'

type Key = DocTypeT
type Data = DocTypeT

data Table = 
    Table T.Text {-the name of the table-} [Key] {-the keys in the row-} [Data] {-the data in the row-}

findCustoms :: M'.Map T.Text CustomT -> DocTypeT -> [T.Text]
findCustoms ecMap (TypeT n, _, _) = 
    (case M'.lookup n ecMap of
        Just (CustomT _ constrs) -> concatMap (\(_,edts) -> concatMap (findCustoms ecMap) edts) constrs 
        Nothing -> error $ "Database plugin generator: Type " ++ T.unpack n ++ " not found in extra types map: " ++ show (M'.keys ecMap))
        ++ [n]
findCustoms ecMap (PairT edt0 edt1, _, _) = findCustoms ecMap edt0 ++ findCustoms ecMap edt1
findCustoms ecMap (TripleT edt0 edt1 edt2, _, _) = findCustoms ecMap edt0 ++ findCustoms ecMap edt1 ++ findCustoms ecMap edt2
findCustoms ecMap (ListT dt, _, _) = findCustoms ecMap dt
findCustoms ecMap (DictT edt0 edt1, _, _) = findCustoms ecMap edt0 ++  findCustoms ecMap edt1
findCustoms ecMap (ExistingT name mod,_,_)    = [T.concat[ mod,".", name]]
findCustoms ecMap (ExistingWParamsT name params mod,_,_) = 
    [T.concat["(", mod,".", name]] ++ (map (\(typ,imp) -> T.concat [imp, ".", typ]) params)
findCustoms ecMap (MaybeT dt, _, _) = findCustoms ecMap dt
findCustoms ecMap (ResultT edt0 edt1, _, _) = findCustoms ecMap edt0 ++ findCustoms ecMap edt1
findCustoms ecMap _ = []

generateDatabase :: [Table] -> M'.Map T.Text CustomT -> Net -> IO [(FilePath, T.Text)]
generateDatabase ts extraTypesMap net =
    let
        netName = getNetName net
        
        generateOneTable (Table name keys values) =
            let
                safecopy n = T.concat ["$(deriveSafeCopy 0 'base ", "''",n,")"]
                safeKeys = 
                    map (\p -> 
                        case p of
                            (TypeT tn, n, d) -> (TypeT (T.concat[getNetName net,".Static.Types.", tn]), n, d)
                            a -> a
                        ) keys
                safeValues = 
                    map (\p -> 
                        case p of
                            (TypeT tn, n, d) -> (TypeT (T.concat[getNetName net,".Static.Types.", tn]), n, d)
                            a -> a
                        ) values
                rowT = ct name $ [constructor name (keys++values)]

                imports = concatMap (findImports Haskell) keys ++ concatMap (findImports Haskell) values
                customs = concatMap (findCustoms extraTypesMap) (keys ++ values)
            in T.unlines 
                [
                    "{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, StandaloneDeriving #-}"
                ,   T.concat["module Plugins.Database.Table.", name," where"]
                ,   "import Data.Data            (Data, Typeable)"
                ,   "import Data.IxSet           (Indexable(..), IxSet(..), (@=), Proxy(..), getOne, ixFun, ixSet)"
                ,   "import Data.SafeCopy        (SafeCopy, base, deriveSafeCopy)"
                ,   "import Static.Types"
                ,   "import Plugins.Database.Query"
                ,   T.concat["import ",netName,".Static.Types"]
                ,   T.unlines imports
                ,   T.unlines $ map (\typ -> T.concat["deriving instance Data ",typ]) (fnub customs)
                ,   T.unlines $ map safecopy (fnub customs)
                ,   ""
                ,   generateType Haskell True False [DOrd,DEq,DShow,DData,DTypeable] rowT
                ,   safecopy name,""
                ,   generateNewtype True [DOrd,DEq,DShow,DData,DTypeable] (T.concat[name,"Record"]) (PairT (TypeT "Index", "index", "") (TypeT name, name, ""))
                ,   safecopy (T.concat[name,"Record"])
                ,   ""
                --,   T.unlines $ map (\(et,n,_) -> generateNewtype True [DOrd,DEq,DShow,DData,DTypeable] (capStr n) et) keys
                --,   T.unlines $ map (\(_,n,_) -> safecopy n) keys
                ]
        main = T.unlines 
                [
                    "module Plugins.Database where"
                ,   T.unlines $ map (\(Table name _ _) -> T.concat ["import Plugins.Database.Table.", name]) ts
                ,   ""
                ]
    in
        return $ [("", main)] ++
            map (\(table@(Table name _ _)) -> ("Table/"++T.unpack name,generateOneTable table)) ts

