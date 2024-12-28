module OpenSolid.Array
  ( Array
  , singleton
  , new
  , items
  , length
  , get
  , map
  , foldl
  , foldr
  )
where

import Data.Array ((!))
import Data.Array qualified
import Data.Foldable qualified
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
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

map :: (a -> b) -> Array a -> Array b
map f (Array n array) = Array n (Prelude.fmap f array)

foldl :: (b -> a -> b) -> b -> Array a -> b
foldl f init (Array _ array) = Data.Foldable.foldl' f init array

foldr :: (a -> b -> b) -> b -> Array a -> b
foldr f init (Array _ array) = Data.Foldable.foldr f init array
