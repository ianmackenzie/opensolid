module OpenSolid.Array
  ( Array
  , singleton
  , fromNonEmpty
  , toNonEmpty
  , toList
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
import Data.Foldable1 (Foldable1)
import Data.Foldable1 qualified as Foldable1
import OpenSolid.Int qualified as Int
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import Prelude (Functor)
import Prelude qualified

newtype Array a = Array (Data.Array.Array Int a) deriving (Eq, Ord, Show)

deriving instance Foldable Array

deriving instance Functor Array

deriving instance Traversable Array

instance Foldable1 Array where
  foldrMap1 f g (Array array) = do
    let n = Prelude.length array
    let init = f (array ! (n - 1))
    foldrMap1Help (n - 2) g init array

  foldlMap1 f g (Array array) = do
    let n = Prelude.length array
    let init = f (array ! 0)
    foldlMap1Help 1 n g init array

  foldrMap1' = Foldable1.foldrMap1
  foldlMap1' = Foldable1.foldlMap1

  toNonEmpty = toNonEmpty
  head = first
  last = last

foldrMap1Help :: Int -> (a -> b -> b) -> b -> Data.Array.Array Int a -> b
foldrMap1Help i g !acc array
  | i >= 0 = foldrMap1Help (i - 1) g (g (array ! i) acc) array
  | otherwise = acc

foldlMap1Help :: Int -> Int -> (b -> a -> b) -> b -> Data.Array.Array Int a -> b
foldlMap1Help i n g !acc array
  | i < n = foldlMap1Help (i + 1) n g (g acc (array ! i)) array
  | otherwise = acc

singleton :: a -> Array a
singleton = fromNonEmpty . NonEmpty.one

fromNonEmpty :: NonEmpty a -> Array a
fromNonEmpty givenItems = do
  let n = NonEmpty.length givenItems
  Array (Data.Array.listArray (0, n - 1) (NonEmpty.toList givenItems))

toNonEmpty :: Array a -> NonEmpty a
toNonEmpty array = case toList array of
  NonEmpty nonEmpty -> nonEmpty
  [] -> internalError "Array should never be empty"

toList :: Array a -> List a
toList (Array array) = Data.Array.elems array

{-# INLINE length #-}
length :: Array a -> Int
length (Array array) = Prelude.length array

{-# INLINE get #-}
get :: Int -> Array a -> a
get index (Array array) = array ! (index % (Prelude.length array))

first :: Array a -> a
first (Array array) = array ! 0

last :: Array a -> a
last (Array array) = array ! (Prelude.length array - 1)

map :: (a -> b) -> Array a -> Array b
map f (Array array) = Array (Prelude.fmap f array)

map2 :: (a -> b -> c) -> Array a -> Array b -> Array c
map2 f (Array array1) (Array array2) = do
  let newItem i = f (array1 ! i) (array2 ! i)
  let n = Int.min (Prelude.length array1) (Prelude.length array2)
  Array (Data.Array.listArray (0, n - 1) (List.map newItem [0 .. n - 1]))

map3 :: (a -> b -> c -> d) -> Array a -> Array b -> Array c -> Array d
map3 f (Array array1) (Array array2) (Array array3) = do
  let newItem i = f (array1 ! i) (array2 ! i) (array3 ! i)
  let n = Int.min (Int.min (Prelude.length array1) (Prelude.length array2)) (Prelude.length array3)
  Array (Data.Array.listArray (0, n - 1) (List.map newItem [0 .. n - 1]))

mapWithIndex :: (Int -> a -> b) -> Array a -> Array b
mapWithIndex f (Array array) = do
  let newItem i = f i (array ! i)
  let n = Prelude.length array
  Array (Data.Array.listArray (0, n - 1) (List.map newItem [0 .. n - 1]))

reverse :: Array a -> Array a
reverse array = do
  let n = length array
  let reversedItems = foldl (\acc item -> item : acc) [] array
  Array (Data.Array.listArray (0, n - 1) reversedItems)

reverseMap :: (a -> b) -> Array a -> Array b
reverseMap f array = do
  let n = length array
  let newItems = foldl (\acc item -> f item : acc) [] array
  Array (Data.Array.listArray (0, n - 1) newItems)

foldl :: (b -> a -> b) -> b -> Array a -> b
foldl f init (Array array) = Prelude.foldl' f init array

foldr :: (a -> b -> b) -> b -> Array a -> b
foldr f init (Array array) = Prelude.foldr f init array
