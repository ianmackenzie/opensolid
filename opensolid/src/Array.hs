module Array
  ( Array
  , length
  , empty
  , initialize
  , fromList
  , toList
  , get
  , map
  , foldLeft
  , foldRight
  )
where

import Data.Array ((!))
import Data.Array qualified
import Data.Foldable qualified
import List qualified
import OpenSolid

newtype Array a = Array (Data.Array.Array Int a)

length :: Array a -> Int
length (Array array) = let (_, i) = Data.Array.bounds array in i + 1

empty :: Array a
empty = Array (Data.Array.array (0, -1) [])

initialize :: Int -> (Int -> a) -> Array a
initialize n function = Array (Data.Array.array (0, n - 1) [(i, function i) | i <- [0 .. n - 1]])

fromList :: List a -> Array a
fromList items = Array (Data.Array.listArray (0, List.length items - 1) items)

toList :: Array a -> List a
toList (Array array) = Data.Array.elems array

get :: Int -> Array a -> Maybe a
get index (Array array) =
  let (_, i) = Data.Array.bounds array
   in if index >= 0 && index <= i
        then Just (array ! index)
        else Nothing

map :: (a -> b) -> Array a -> Array b
map function (Array array) = Array (fmap function array)

foldLeft :: (a -> b -> b) -> b -> Array a -> b
foldLeft function init (Array array) = Data.Foldable.foldl' (\acc item -> function item acc) init array

foldRight :: (a -> b -> b) -> b -> Array a -> b
foldRight function init (Array array) = Data.Foldable.foldr function init array
