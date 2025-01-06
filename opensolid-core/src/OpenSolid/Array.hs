module OpenSolid.Array
  ( Array
  , singleton
  , new
  , items
  , length
  , get
  , first
  , last
  , map
  , map2
  , map3
  , mapWithIndex
  , reverse
  , reverseMap
  , foldl
  , foldr
  )
where

import Data.Array ((!))
import Data.Array qualified
import Data.Foldable qualified
import OpenSolid.Int qualified as Int
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import Prelude qualified

data Array a = Array Int (Data.Array.Array Int a) deriving (Show)

singleton :: a -> Array a
singleton = new . NonEmpty.one

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

map2 :: (a -> b -> c) -> Array a -> Array b -> Array c
map2 f (Array n1 array1) (Array n2 array2) = do
  let newItem i = f (array1 ! i) (array2 ! i)
  let n = Int.min n1 n2
  Array n (Data.Array.listArray (0, n - 1) (List.map newItem [0 .. n - 1]))

map3 :: (a -> b -> c -> d) -> Array a -> Array b -> Array c -> Array d
map3 f (Array n1 array1) (Array n2 array2) (Array n3 array3) = do
  let newItem i = f (array1 ! i) (array2 ! i) (array3 ! i)
  let n = Int.min (Int.min n1 n2) n3
  Array n (Data.Array.listArray (0, n - 1) (List.map newItem [0 .. n - 1]))

mapWithIndex :: (Int -> a -> b) -> Array a -> Array b
mapWithIndex f (Array n array) = do
  let newItem i = f i (array ! i)
  Array n (Data.Array.listArray (0, n - 1) (List.map newItem [0 .. n - 1]))

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
