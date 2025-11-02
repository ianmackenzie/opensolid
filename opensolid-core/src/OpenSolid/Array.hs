module OpenSolid.Array
  ( Array
  , empty
  , singleton
  , fromList
  , fromNonEmpty
  , toList
  , initialize
  , get
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
import OpenSolid.Int qualified as Int
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude hiding (foldl, foldr)
import Prelude qualified

newtype Array a = Array (Data.Array.Array Int a) deriving (Eq, Ord, Show)

deriving instance Foldable Array

deriving instance Functor Array

deriving instance Traversable Array

empty :: Array a
empty = fromList []

singleton :: a -> Array a
singleton value = fromList [value]

fromList :: List a -> Array a
fromList list = Array (Data.Array.listArray (0, list.length - 1) list)

fromNonEmpty :: NonEmpty a -> Array a
fromNonEmpty = fromList . NonEmpty.toList

toList :: Array a -> List a
toList (Array array) = Data.Array.elems array

initialize :: Int -> (Int -> a) -> Array a
initialize n function
  | n <= 0 = empty
  | otherwise = Array (Data.Array.listArray (0, n - 1) (List.map function [0 .. n - 1]))

instance HasField "length" (Array a) Int where
  getField (Array array) = Prelude.length array

{-# INLINE get #-}
get :: Int -> Array a -> a
get index (Array array) = array ! index

map :: (a -> b) -> Array a -> Array b
map f (Array array) = Array (Prelude.fmap f array)

map2 :: (a -> b -> c) -> Array a -> Array b -> Array c
map2 f array1 array2 =
  initialize (Int.min array1.length array2.length) (\i -> f (get i array1) (get i array2))

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
  let n = array.length
  let reversedItems = foldl (\acc item -> item : acc) [] array
  Array (Data.Array.listArray (0, n - 1) reversedItems)

reverseMap :: (a -> b) -> Array a -> Array b
reverseMap f array = do
  let n = array.length
  let newItems = foldl (\acc item -> f item : acc) [] array
  Array (Data.Array.listArray (0, n - 1) newItems)

foldl :: (b -> a -> b) -> b -> Array a -> b
foldl f init (Array array) = Prelude.foldl' f init array

foldr :: (a -> b -> b) -> b -> Array a -> b
foldr f init (Array array) = Prelude.foldr f init array
