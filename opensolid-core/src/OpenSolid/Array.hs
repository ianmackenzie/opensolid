module OpenSolid.Array
  ( Array
  , empty
  , singleton
  , fromList
  , fromNonEmpty
  , toList
  , initialize
  , length
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

import Data.Array qualified
import OpenSolid.IndexOutOfBounds (IndexOutOfBounds (..))
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import Prelude qualified

newtype Array a = Array (Data.Array.Array Int a) deriving (Eq, Ord, Show)

deriving instance Foldable Array

deriving instance Functor Array

deriving instance Traversable Array

instance Indexed (Array a) Int a where
  {-# INLINE (!!) #-}
  array !! index
    | index >= 0 && index < length array = unwrap array Data.Array.! index
    | otherwise = throw IndexOutOfBounds{index = index, size = length array}

{-# INLINE unwrap #-}
unwrap :: Array a -> Data.Array.Array Int a
unwrap (Array array) = array

empty :: Array a
empty = fromList []

singleton :: a -> Array a
singleton value = fromList [value]

fromList :: List a -> Array a
fromList list = Array (Data.Array.listArray (0, List.length list - 1) list)

fromNonEmpty :: NonEmpty a -> Array a
fromNonEmpty = fromList . NonEmpty.toList

toList :: Array a -> List a
toList array = Data.Array.elems (unwrap array)

initialize :: Int -> (Int -> a) -> Array a
initialize n function
  | n <= 0 = empty
  | otherwise = Array (Data.Array.listArray (0, n - 1) (List.map function [0 .. n - 1]))

length :: Array a -> Int
length array = Prelude.length (unwrap array)

map :: (a -> b) -> Array a -> Array b
map = Prelude.fmap

map2 :: (a -> b -> c) -> Array a -> Array b -> Array c
map2 f array1 array2 = do
  let n = min (length array1) (length array2)
  initialize n $ \i -> f (array1 !! i) (array2 !! i)

map3 :: (a -> b -> c -> d) -> Array a -> Array b -> Array c -> Array d
map3 f array1 array2 array3 = do
  let n = length array1 `min` length array2 `min` length array3
  initialize n $ \i -> f (array1 !! i) (array2 !! i) (array3 !! i)

mapWithIndex :: (Int -> a -> b) -> Array a -> Array b
mapWithIndex f array = initialize (length array) $ \i -> f i (array !! i)

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
foldl f init array = Prelude.foldl' f init (unwrap array)

foldr :: (a -> b -> b) -> b -> Array a -> b
foldr f init array = Prelude.foldr f init (unwrap array)
