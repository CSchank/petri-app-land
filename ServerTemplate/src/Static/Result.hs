module Static.Result where

import Data.SafeCopy (SafeCopy)

data Result err ok =
      Ok ok
    | Err err
    deriving(Eq,Ord,Show)

instance Functor (Result err) where
    fmap = Static.Result.map

instance Applicative (Result err) where
    pure res = Ok res
    Err err <*> _ = Err err
    Ok f <*> r = fmap f r

instance Monad (Result err) where
   return = pure
   (>>=) = flip Static.Result.andThen

map :: (a -> value) -> Result x a -> Result x value
map fn ra =
    case ra of 
        Err x -> Err x
        Ok a -> Ok $ fn a

mapError :: (x -> y) -> Result x a -> Result y a
mapError fn rx =
    case rx of
        Ok a -> Ok a
        Err x -> Err $ fn x 

map1 :: (a -> value) -> Result x a -> Result x value
map1 = Static.Result.map


map2 :: (a -> b -> value) -> Result x a -> Result x b -> Result x value
map2 fn ra rb = fn <$> ra <*> rb

map3 :: (a -> b -> c -> value) -> Result x a -> Result x b -> Result x c -> Result x value
map3 fn ra rb rc = fn <$> ra <*> rb <*> rc

map4 :: (a -> b -> c -> d -> value) -> Result x a -> Result x b -> Result x c -> Result x d -> Result x value
map4 fn ra rb rc rd = fn <$> ra <*> rb <*> rc <*> rd

map5 :: (a -> b -> c -> d -> e -> value) -> Result x a -> Result x b -> Result x c -> Result x d -> Result x e -> Result x value
map5 fn ra rb rc rd re = fn <$> ra <*> rb <*> rc <*> rd <*> re

map6 :: (a -> b -> c -> d -> e -> f -> value) -> Result x a -> Result x b -> Result x c -> Result x d -> Result x e -> Result x f -> Result x value
map6 fn ra rb rc rd re rf = fn <$> ra <*> rb <*> rc <*> rd <*> re <*> rf

map7 :: (a -> b -> c -> d -> e -> f -> g -> value) -> Result x a -> Result x b -> Result x c -> Result x d -> Result x e -> Result x f -> Result x g -> Result x value
map7 fn ra rb rc rd re rf rg = fn <$> ra <*> rb <*> rc <*> rd <*> re <*> rf <*> rg

map8 :: (a -> b -> c -> d -> e -> f -> g -> h -> value)
      -> Result x a
      -> Result x b
      -> Result x c
      -> Result x d
      -> Result x e
      -> Result x f
      -> Result x g
      -> Result x h 
      -> Result x value
map8 fn ra rb rc rd re rf rg rh = fn <$> ra <*> rb <*> rc <*> rd <*> re <*> rf <*> rg <*> rh

map9 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> value)
      -> Result x a
      -> Result x b
      -> Result x c
      -> Result x d
      -> Result x e
      -> Result x f
      -> Result x g
      -> Result x h
      -> Result x i
      -> Result x value
map9 fn ra rb rc rd re rf rg rh ri = fn <$> ra <*> rb <*> rc <*> rd <*> re <*> rf <*> rg <*> rh <*> ri

map10 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> value)
      -> Result x a
      -> Result x b
      -> Result x c
      -> Result x d
      -> Result x e
      -> Result x f
      -> Result x g
      -> Result x h
      -> Result x i
      -> Result x j
      -> Result x value
map10 fn ra rb rc rd re rf rg rh ri rj =
  fn <$> ra <*> rb <*> rc <*> rd <*> re <*> rf <*> rg <*> rh <*> ri <*> rj

map11 fn r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 =
  fn <$> r1 <*> r2 <*> r3 <*> r4 <*> r5 <*> r6 <*> r7 <*> r8 <*> r9 <*> r10 <*> r11

map12 fn r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 =
  fn <$> r1 <*> r2 <*> r3 <*> r4 <*> r5 <*> r6 <*> r7 <*> r8 <*> r9 <*> r10 <*> r11 <*> r12

map13 fn r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 =
  fn <$> r1 <*> r2 <*> r3 <*> r4 <*> r5 <*> r6 <*> r7 <*> r8 <*> r9 <*> r10 <*> r11 <*> r12 <*> r13

map14 fn r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 =
  fn <$> r1 <*> r2 <*> r3 <*> r4 <*> r5 <*> r6 <*> r7 <*> r8 <*> r9 <*> r10 <*> r11 <*> r12 <*> r13 <*> r14

map15 fn r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15  =
  fn <$> r1 <*> r2 <*> r3 <*> r4 <*> r5 <*> r6 <*> r7 <*> r8 <*> r9 <*> r10 <*> r11 <*> r12 <*> r13 <*> r14 <*> r15

andThen :: (a -> Result x b) -> Result x a -> Result x b
andThen callback result =
    case result of
      Ok value ->
        callback value

      Err msg ->
        Err msg

withDefault :: a -> Result x a -> a
withDefault def result =
  case result of
    Ok a ->
        a

    Err _ ->
        def
