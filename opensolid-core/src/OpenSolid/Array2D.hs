module OpenSolid.Array2D
  ( Array2D
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
import OpenSolid.List qualified as List
import OpenSolid.Prelude
import Prelude qualified

data Array2D a = Array2D (Int, Int) (Data.Array.Array (Int, Int) a) deriving (Show, Functor)

singleton :: a -> Array2D a
singleton item = Array2D (1, 1) (Data.Array.listArray ((0, 0), (0, 0)) [item])

new :: (a -> b -> c) -> Array a -> Array b -> Array2D c
new f uItems vItems = do
  let n = Array.length uItems
  let m = Array.length vItems
  let item i = f (Array.get (i // m) uItems) (Array.get (i % m) vItems)
  let items = List.map item [0 .. n * m - 1]
  Array2D (n, m) (Data.Array.listArray ((0, 0), (n - 1, m - 1)) items)

dimensions :: Array2D a -> (Int, Int)
dimensions (Array2D dims _) = dims

{-# INLINE get #-}
get :: (Int, Int) -> Array2D a -> a
get indices (Array2D _ array) = array ! indices

map :: (a -> b) -> Array2D a -> Array2D b
map = Prelude.fmap

map2 :: (a -> b -> c) -> Array2D a -> Array2D b -> Array2D c
map2 f (Array2D (n1, m1) array1) (Array2D (n2, m2) array2) = do
  let n = min n1 n2
  let m = min m1 m2
  let newItem index = do
        let i = index // m
        let j = index % m
        let indices = (i, j)
        f (array1 ! indices) (array2 ! indices)
  let newItems = List.map newItem [0 .. n * m - 1]
  Array2D (n, m) (Data.Array.listArray ((0, 0), (n - 1, m - 1)) newItems)
