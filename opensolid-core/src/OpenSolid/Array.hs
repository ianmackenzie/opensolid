module OpenSolid.Array
  ( Array
  , singleton
  , new
  , items
  , length
  , get
  , first
  , last
  , interpolate
  , aggregate
  , map
  , reverse
  , reverseMap
  , foldl
  , foldr
  )
where

import Data.Array ((!))
import Data.Array qualified
import Data.Foldable qualified
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Float qualified as Float
import OpenSolid.Int qualified as Int
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Range (Range)
import {-# SOURCE #-} OpenSolid.Range qualified as Range
import Prelude qualified

data Array a = Array Int (Data.Array.Array Int a) deriving (Show)

singleton :: a -> Array a
singleton = new . NonEmpty.singleton

new :: NonEmpty a -> Array a
new givenItems = do
  let n = NonEmpty.length givenItems
  Array n (Data.Array.listArray (0, n - 1) (NonEmpty.toList givenItems))

items :: Array a -> NonEmpty a
items (Array _ array) =
  case Data.Array.elems array of
    NonEmpty arrayItems -> arrayItems
    [] -> internalError "Array should never be empty"

{-# INLINE length #-}
length :: Array a -> Int
length (Array n _) = n

{-# INLINE get #-}
get :: Int -> Array a -> a
get index (Array n array) = array ! (index % n)

first :: Array a -> a
first (Array _ array) = array ! 0

last :: Array a -> a
last (Array n array) = array ! (n - 1)

map :: (a -> b) -> Array a -> Array b
map f (Array n array) = Array n (Prelude.fmap f array)

reverse :: Array a -> Array a
reverse array = do
  let n = length array
  let reversedItems = foldl (\acc item -> item : acc) [] array
  Array n (Data.Array.listArray (0, n - 1) reversedItems)

reverseMap :: (a -> b) -> Array a -> Array b
reverseMap f array = do
  let n = length array
  let newItems = foldl (\acc item -> f item : acc) [] array
  Array n (Data.Array.listArray (0, n - 1) newItems)

foldl :: (b -> a -> b) -> b -> Array a -> b
foldl f init (Array _ array) = Data.Foldable.foldl' f init array

foldr :: (a -> b -> b) -> b -> Array a -> b
foldr f init (Array _ array) = Data.Foldable.foldr f init array

{-# INLINEABLE position #-}
position :: Int -> Float -> (# Int, Float #)
position n t = do
  let n' = Float.int n
  let i = Int.clampTo (0, n - 1) (Float.floor (t * n'))
  let t0 = Float.int i / n'
  (# i, (t - t0) * n' #)

{-# INLINEABLE interpolate #-}
interpolate :: (a -> Float -> b) -> Array a -> Float -> b
interpolate f (Array n array) t = do
  let (# i, ti #) = position n t
  f (array ! i) ti

aggregate :: Bounds.Interface b => (a -> Range Unitless -> b) -> Array a -> Range Unitless -> b
aggregate f array t = do
  let n = length array
  let (# i, ti #) = position n (Range.lowerBound t)
  let (# j, tj #) = position n (Range.upperBound t)
  if i == j
    then f (get i array) (Range.from ti tj)
    else do
      let bi = f (get i array) (Range.from ti 1.0)
      let bj = f (get j array) (Range.from 0.0 tj)
      aggregateImpl f array (i + 1) (j - 1) (Bounds.aggregate2 bi bj)

aggregateImpl :: Bounds.Interface b => (a -> Range Unitless -> b) -> Array a -> Int -> Int -> b -> b
aggregateImpl f array i j acc
  | i <= j = do
      let bi = f (get i array) Range.unit
      aggregateImpl f array (i + 1) j (Bounds.aggregate2 acc bi)
  | otherwise = acc
