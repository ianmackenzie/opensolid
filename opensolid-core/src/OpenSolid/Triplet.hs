module OpenSolid.Triplet
  ( first
  , second
  , third
  , map
  , map2
  , mapFirst
  , mapSecond
  , mapThird
  )
where

{-# INLINE first #-}
first :: (a, b, c) -> a
first (a, _, _) = a

{-# INLINE second #-}
second :: (a, b, c) -> b
second (_, b, _) = b

{-# INLINE third #-}
third :: (a, b, c) -> c
third (_, _, c) = c

{-# INLINE map #-}
map :: (a -> b) -> (a, a, a) -> (b, b, b)
map f (a1, a2, a3) = (f a1, f a2, f a3)

map2 :: (a -> b -> c) -> (a, a, a) -> (b, b, b) -> (c, c, c)
map2 f (a1, a2, a3) (b1, b2, b3) = (f a1 b1, f a2 b2, f a3 b3)

{-# INLINE mapFirst #-}
mapFirst :: (a1 -> a2) -> (a1, b, c) -> (a2, b, c)
mapFirst f (a, b, c) = (f a, b, c)

{-# INLINE mapSecond #-}
mapSecond :: (b1 -> b2) -> (a, b1, c) -> (a, b2, c)
mapSecond f (a, b, c) = (a, f b, c)

{-# INLINE mapThird #-}
mapThird :: (c1 -> c2) -> (a, b, c1) -> (a, b, c2)
mapThird f (a, b, c) = (a, b, f c)
