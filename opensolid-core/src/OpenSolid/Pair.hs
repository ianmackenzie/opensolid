module OpenSolid.Pair
  ( first
  , second
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

mapFirst :: (a1 -> a2) -> (a1, b) -> (a2, b)
mapFirst f (a, b) = (f a, b)

mapSecond :: (b1 -> b2) -> (a, b1) -> (a, b2)
mapSecond f (a, b) = (a, f b)

decorate :: (a -> b) -> a -> (a, b)
decorate function item = (item, function item)
