module Array
  ( Array
  , length
  , empty
  , initialize
  , fromList
  , toList
  , get
  , map
  , foldl
  , foldr
  )
where

import Data.Array ((!))
import Data.Array qualified
import Data.Foldable qualified
import List qualified
import OpenSolid
import Prelude qualified

type Array a = Data.Array.Array Int a

length :: Array a -> Int
length array = do
  let (_, i) = Data.Array.bounds array
  i + 1

empty :: Array a
empty = Data.Array.array (0, -1) []

initialize :: Int -> (Int -> a) -> Array a
initialize n function = Data.Array.array (0, n - 1) [(i, function i) | i <- [0 .. n - 1]]

fromList :: List a -> Array a
fromList items = Data.Array.listArray (0, List.length items - 1) items

toList :: Array a -> List a
toList = Data.Array.elems

get :: Int -> Array a -> Maybe a
get index array = do
  let (_, i) = Data.Array.bounds array
  if index >= 0 && index <= i
    then Just (array ! index)
    else Nothing

map :: (a -> b) -> Array a -> Array b
map = Prelude.fmap

foldl :: (b -> a -> b) -> b -> Array a -> b
foldl = Data.Foldable.foldl'

foldr :: (a -> b -> b) -> b -> Array a -> b
foldr = Data.Foldable.foldr
