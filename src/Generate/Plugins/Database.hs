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
    Table String {-the name of the table-} [Key] {-the keys in the row-} [Data] {-the data in the row-}

findCustoms :: M'.Map String CustomT -> DocTypeT -> [T.Text]
findCustoms ecMap (TypeT n, _, _) = 
    (case M'.lookup n ecMap of
        Just (CustomT _ constrs) -> concatMap (\(_,edts) -> concatMap (findCustoms ecMap) edts) constrs 
        Nothing -> error $ "Database plugin generator: Type " ++ n ++ " not found in extra types map: " ++ show (M'.keys ecMap))
        ++ [T.pack n]
findCustoms ecMap (PairT edt0 edt1, _, _) = findCustoms ecMap edt0 ++ findCustoms ecMap edt1
findCustoms ecMap (TripleT edt0 edt1 edt2, _, _) = findCustoms ecMap edt0 ++ findCustoms ecMap edt1 ++ findCustoms ecMap edt2
findCustoms ecMap (ListT edt, _, _) = findCustoms ecMap edt
findCustoms ecMap (DictT edt0 edt1, _, _) = findCustoms ecMap edt0 ++  findCustoms ecMap edt1
findCustoms ecMap (ExistingT name mod,_,_)    = [T.concat[T.pack mod,".",T.pack name]]
findCustoms ecMap (ExistingWParamsT name params mod,_,_) = 
    [T.concat["(",T.pack mod,".",T.pack name]] ++ (map (\(typ,imp) -> T.pack $ imp ++ "." ++ typ) params)
findCustoms ecMap (MaybeT edt, _, _) = findCustoms ecMap edt
findCustoms ecMap (ResultT edt0 edt1, _, _) = findCustoms ecMap edt0 ++ findCustoms ecMap edt1
findCustoms ecMap _ = []

generateDatabase :: [Table] -> M'.Map String CustomT -> Net -> IO [(FilePath, T.Text)]
generateDatabase ts extraTypesMap net =
    let
        netName = getNetName net
        
        generateOneTable (Table name keys values) =
            let
                safecopy n = T.concat ["$(deriveSafeCopy 0 'base ", "''",T.pack $ n,")"]
                safeKeys = 
                    map (\p -> 
                        case p of
                            (TypeT tn, n, d) -> (TypeT (T.unpack (getNetName net) ++ ".Static.Types." ++ tn), n, d)
                            a -> a
                        ) keys
                safeValues = 
                    map (\p -> 
                        case p of
                            (TypeT tn, n, d) -> (TypeT (T.unpack (getNetName net) ++ ".Static.Types." ++ tn), n, d)
                            a -> a
                        ) values
                rowT = ec name $ [constructor name (keys++values)]

                imports = concatMap (findImports Haskell) keys ++ concatMap (findImports Haskell) values
                customs = concatMap (findCustoms extraTypesMap) (keys ++ values)
            in T.unlines 
                [
                    "{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, StandaloneDeriving #-}"
                ,   T.concat["module Plugins.Database.Table.",T.pack name," where"]
                ,   "import Data.Data            (Data, Typeable)"
                ,   "import Data.IxSet           (Indexable(..), IxSet(..), (@=), Proxy(..), getOne, ixFun, ixSet)"
                ,   "import Data.SafeCopy        (SafeCopy, base, deriveSafeCopy)"
                ,   "import Static.Types"
                ,   "import Plugins.Database.Query"
                ,   T.concat["import ",netName,".Static.Types"]
                ,   T.unlines imports
                ,   T.unlines $ map (\typ -> T.concat["deriving instance Data ",typ]) (fnub customs)
                ,   T.unlines $ map (safecopy . T.unpack) (fnub customs)
                ,   ""
                ,   generateType Haskell True [DOrd,DEq,DShow,DData,DTypeable] rowT
                ,   safecopy name,""
                ,   generateNewtype True [DOrd,DEq,DShow,DData,DTypeable] (name++"Record") (PairT (TypeT "Index", "index", "") (TypeT name, name, ""))
                ,   safecopy (name++"Record")
                ,   ""
                --,   T.unlines $ map (\(et,n,_) -> generateNewtype True [DOrd,DEq,DShow,DData,DTypeable] (capStr n) et) keys
                --,   T.unlines $ map (\(_,n,_) -> safecopy n) keys
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

