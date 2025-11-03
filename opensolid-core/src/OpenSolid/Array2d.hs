module OpenSolid.Array2d
  ( Array2d
  , singleton
  , new
  , dimensions
  , get
  , map
  , map2
  )
where

import Data.Array ((!))
import Data.Array qualified
import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.Int qualified as Int
import OpenSolid.List qualified as List
import OpenSolid.Prelude

data Array2d a = Array2d (Int, Int) (Data.Array.Array (Int, Int) a) deriving (Show)

singleton :: a -> Array2d a
singleton item = Array2d (1, 1) (Data.Array.listArray ((0, 0), (0, 0)) [item])

new :: (a -> b -> c) -> Array a -> Array b -> Array2d c
new f uItems vItems = do
  let n = uItems.length
  let m = vItems.length
  let item i = f (Array.get (i // m) uItems) (Array.get (i % m) vItems)
  let items = List.map item [0 .. n * m - 1]
  Array2d (n, m) (Data.Array.listArray ((0, 0), (n - 1, m - 1)) items)

dimensions :: Array2d a -> (Int, Int)
dimensions (Array2d dims _) = dims

{-# INLINE get #-}
get :: (Int, Int) -> Array2d a -> a
get indices (Array2d _ array) = array ! indices

map :: (a -> b) -> Array2d a -> Array2d b
map f (Array2d dims array) = Array2d dims (fmap f array)

map2 :: (a -> b -> c) -> Array2d a -> Array2d b -> Array2d c
map2 f (Array2d (n1, m1) array1) (Array2d (n2, m2) array2) = do
  let n = Int.min n1 n2
  let m = Int.min m1 m2
  let newItem index = do
        let i = index // m
        let j = index % m
        let indices = (i, j)
        f (array1 ! indices) (array2 ! indices)
  let newItems = List.map newItem [0 .. n * m - 1]
  Array2d (n, m) (Data.Array.listArray ((0, 0), (n - 1, m - 1)) newItems)
