module OpenSolid.Pair
  ( first
  , second
  , map
  , mapFirst
  , mapSecond
  , decorate
  )
where

{-# INLINE first #-}
first :: (a, b) -> a
first (a, _) = a

{-# INLINE second #-}
second :: (a, b) -> b
second (_, b) = b

{-# INLINE map #-}
map :: (a -> b) -> (a, a) -> (b, b)
map f (a1, a2) = (f a1, f a2)

mapFirst :: (a1 -> a2) -> (a1, b) -> (a2, b)
mapFirst f (a, b) = (f a, b)

mapSecond :: (b1 -> b2) -> (a, b1) -> (a, b2)
mapSecond f (a, b) = (a, f b)

decorate :: (a -> b) -> a -> (a, b)
decorate function item = (item, function item)
