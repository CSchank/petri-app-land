module Plugins.Database.Query where
import Data.Data (Data, Typeable)
import Data.IxSet ( Indexable(..), IxSet(..), Proxy(..), getOne
                  , ixFun, ixSet, getEQ, getLT, getGT, getLTE, getGTE
                  , union, intersection )


{-data Query a key where 
      QEq :: (a -> key) -> a -> Query a key
      QLT :: (a -> key) -> a -> Query a key
      QGT :: (a -> key) -> a -> Query a key
      QLTE :: (a -> key) -> a -> Query a key
      QGTE :: (a -> key) -> a -> Query a key
      QLTGT :: (a -> key) -> (a,a) -> Query a key
      QLTGTE :: (a -> key) -> (a,a) -> Query a key
      QLTEGT :: (a -> key) -> (a,a) -> Query a key
      QLTEGTE :: (a -> key) -> (a,a) -> Query a key-}
{-data Query a key =
      Eq (a -> key) a
    | Lt (a -> key) a 
    | Gt (a -> key) a
    | LtE (a -> key) a
    | GtE (a -> key) a 
    | LtGt (a -> key) (a,a)
    | LtGtE (a -> key) (a,a)
    | LtEGt (a -> key) (a,a)
    | LtEGtE (a -> key) (a,a)
    | Union (Query a key) (Query a key)
    | Intersection (Query a key) (Query a key)-}

type Query r = IxSet r -> IxSet r

(@==) :: (Indexable r, Typeable r, Ord r, Typeable key) => (a -> key) -> a -> Query r
(@==) keyFn a =
    getEQ (keyFn a)

(@<) :: (Indexable r, Typeable r, Ord r, Typeable key) => (a -> key) -> a -> Query r
(@<) keyFn a =
    getLT (keyFn a)

(@>) :: (Indexable r, Typeable r, Ord r, Typeable key) => (a -> key) -> a -> Query r
(@>) keyFn a =
    getGT (keyFn a)

(@<=) :: (Indexable r, Typeable r, Ord r, Typeable key) => (a -> key) -> a -> Query r
(@<=) keyFn a =
    getLTE (keyFn a)

(@>=) :: (Indexable r, Typeable r, Ord r, Typeable key) => (a -> key) -> a -> Query r
(@>=) keyFn a =
    getGTE (keyFn a)

(@><) :: (Indexable r, Typeable r, Ord r, Typeable key) => (a -> key) -> (a,a) -> Query r
(@><) keyFn (a,b) =
    getGT (keyFn a) . getLT (keyFn b)

(@><=) :: (Indexable r, Typeable r, Ord r, Typeable key) => (a -> key) -> (a,a) -> Query r
(@><=) keyFn (a,b) =
    getGT (keyFn a) . getLTE (keyFn b)

(@=><) :: (Indexable r, Typeable r, Ord r, Typeable key) => (a -> key) -> (a,a) -> Query r
(@=><) keyFn (a,b) =
    getGTE (keyFn a) . getLT (keyFn b)

(@=><=) :: (Indexable r, Typeable r, Ord r, Typeable key) => (a -> key) -> (a,a) -> Query r
(@=><=) keyFn (a,b) =
    getGTE (keyFn a) . getLTE (keyFn b)

(&&&) :: (Indexable r, Typeable r, Ord r) => Query r -> Query r -> Query r
(&&&) q0 q1 =
    \ix -> intersection (q0 ix) (q1 ix)

(|||) :: (Indexable r, Typeable r, Ord r) => Query r -> Query r -> Query r
(|||) q0 q1 =
    \ix -> union (q0 ix) (q1 ix)

{-
(@==) :: Typeable key => (a -> key) -> a -> Query r
(@==) keyFn a =
    Eq keyFn a

(@<) :: Typeable key => (a -> key) -> a -> Query r
(@<) keyFn a =
    Lt keyFn a

(@>) :: Typeable key => (a -> key) -> a -> Query r
(@>) keyFn a =
    Gt keyFn a

(@<=) :: Typeable key => (a -> key) -> a -> Query r
(@<=) keyFn a =
    LtE keyFn a

(@>=) :: Typeable key => (a -> key) -> a -> Query r
(@>=) keyFn a =
    GtE keyFn a

(@><) :: Typeable key => (a -> key) -> (a,a) -> Query r
(@><) keyFn (a,b) =
    LtGt keyFn (a,b)

(@><=) :: Typeable key => (a -> key) -> (a,a) -> Query r
(@><=) keyFn (a,b) =
    LtEGt keyFn (a,b)

(@=><) :: Typeable key => (a -> key) -> (a,a) -> Query r
(@=><) keyFn (a,b) =
    LtGtE keyFn (a,b)

(@=><=) :: Typeable key => (a -> key) -> (a,a) -> Query r
(@=><=) keyFn (a,b) =
    LtEGtE keyFn (a,b)

(&&&) :: Typeable key => Query a key -> Query a key -> Query r
(&&&) q0 q1 = Intersection q0 q1

(|||) :: Typeable key => Query a key -> Query a key -> Query r
(|||) q0 q1 = Union q0 q1-}