module Static.Plugins.Database.Query where
import Data.Data (Data, Typeable)
import Data.IxSet ( Indexable(..), IxSet(..), Proxy(..), getOne
                  , ixFun, ixSet, getEQ, getLT, getGT, getLTE, getGTE )

(@==) :: (Indexable r, Typeable r, Ord r, Typeable key) => (a -> key) -> a -> (IxSet r -> IxSet r)
(@==) keyFn a =
    getEQ (keyFn a)

(@<) :: (Indexable r, Typeable r, Ord r, Typeable key) => (a -> key) -> a -> (IxSet r -> IxSet r)
(@<) keyFn a =
    getLT (keyFn a)

(@>) :: (Indexable r, Typeable r, Ord r, Typeable key) => (a -> key) -> a -> (IxSet r -> IxSet r)
(@>) keyFn a =
    getGT (keyFn a)

(@<=) :: (Indexable r, Typeable r, Ord r, Typeable key) => (a -> key) -> a -> (IxSet r -> IxSet r)
(@<=) keyFn a =
    getLTE (keyFn a)

(@>=) :: (Indexable r, Typeable r, Ord r, Typeable key) => (a -> key) -> a -> (IxSet r -> IxSet r)
(@>=) keyFn a =
    getGTE (keyFn a)

(@><) :: (Indexable r, Typeable r, Ord r, Typeable key) => ((a,a) -> key) -> (a,a) -> (IxSet r -> IxSet r)
(@><) keyFn (a,b) =
    getGT (keyFn a) . getLT (keyFn b)

(@><=) :: (Indexable r, Typeable r, Ord r, Typeable key) => ((a,a) -> key) -> (a,a) -> (IxSet r -> IxSet r)
(@><=) keyFn (a,b) =
    getGT (keyFn a) . getLTE (keyFn b)

(@=><) :: (Indexable r, Typeable r, Ord r, Typeable key) => ((a,a) -> key) -> (a,a) -> (IxSet r -> IxSet r)
(@=><) keyFn (a,b) =
    getGTE (keyFn a) . getLT (keyFn b)

(@=><=) :: (Indexable r, Typeable r, Ord r, Typeable key) => ((a,a) -> key) -> (a,a) -> (IxSet r -> IxSet r)
(@=><=) keyFn (a,b) =
    getGTE (keyFn a) . getLTE (keyFn b)