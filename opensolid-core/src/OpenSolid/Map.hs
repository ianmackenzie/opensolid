module OpenSolid.Map
  ( Map
  , empty
  , isEmpty
  , size
  , singleton
  , fromKeyValuePairs
  , toList
  , get
  , take
  , set
  , update
  , remove
  , keys
  , values
  , filter
  , filterWithKey
  , map
  , mapWithKey
  , merge
  , mergeWithKey
  , getMin
  , takeMin
  , getMax
  , takeMax
  , foldl
  , foldlWithKey
  , foldr
  , foldrWithKey
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified
import OpenSolid.Bootstrap
import OpenSolid.Pair qualified as Pair
import Prelude qualified

empty :: Map k v
empty = Data.Map.Strict.empty

isEmpty :: Map k v -> Bool
isEmpty = Data.Map.Strict.null

size :: Map k v -> Int
size = Data.Map.Strict.size

singleton :: k -> v -> Map k v
singleton = Data.Map.Strict.singleton

fromKeyValuePairs :: Ord k => List (k, v) -> Map k v
fromKeyValuePairs = Data.Map.Strict.fromList

toList :: Map k v -> List (k, v)
toList = Data.Map.Strict.toList

get :: Ord k => k -> Map k v -> Maybe v
get = Data.Map.Strict.lookup

take :: Ord k => k -> Map k v -> (Maybe v, Map k v)
take = Data.Map.Strict.updateLookupWithKey (\_ _ -> Nothing)

set :: Ord k => k -> v -> Map k v -> Map k v
set = Data.Map.Strict.insert

update :: Ord k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
update = Data.Map.Strict.alter

remove :: Ord k => k -> Map k v -> Map k v
remove = Data.Map.Strict.delete

keys :: Map k v -> List k
keys = Data.Map.Strict.keys

values :: Map k v -> List v
values = Data.Map.Strict.elems

filter :: (v -> Bool) -> Map k v -> Map k v
filter = Data.Map.Strict.filter

filterWithKey :: (k -> v -> Bool) -> Map k v -> Map k v
filterWithKey = Data.Map.Strict.filterWithKey

map :: (v -> w) -> Map k v -> Map k w
map = Prelude.fmap

mapWithKey :: (k -> v -> w) -> Map k v -> Map k w
mapWithKey = Data.Map.Strict.mapWithKey

merge :: Ord k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
merge = Data.Map.Strict.unionWith

mergeWithKey :: Ord k => (k -> v -> v -> v) -> Map k v -> Map k v -> Map k v
mergeWithKey = Data.Map.Strict.unionWithKey

getMin :: Ord k => Map k v -> Maybe (k, v)
getMin givenMap = if isEmpty givenMap then Nothing else Just (Data.Map.Strict.findMin givenMap)

getMax :: Ord k => Map k v -> Maybe (k, v)
getMax givenMap = if isEmpty givenMap then Nothing else Just (Data.Map.Strict.findMax givenMap)

takeMin :: Ord k => Map k v -> (Maybe (k, v), Map k v)
takeMin givenMap =
  if isEmpty givenMap
    then (Nothing, givenMap)
    else Pair.mapFirst Just (Data.Map.Strict.deleteFindMin givenMap)

takeMax :: Ord k => Map k v -> (Maybe (k, v), Map k v)
takeMax givenMap =
  if isEmpty givenMap
    then (Nothing, givenMap)
    else Pair.mapFirst Just (Data.Map.Strict.deleteFindMax givenMap)

foldr :: (v -> a -> a) -> a -> Map k v -> a
foldr = Data.Map.Strict.foldr

foldrWithKey :: (k -> v -> a -> a) -> a -> Map k v -> a
foldrWithKey = Data.Map.Strict.foldrWithKey

foldl :: (a -> v -> a) -> a -> Map k v -> a
foldl = Data.Map.Strict.foldl'

foldlWithKey :: (a -> k -> v -> a) -> a -> Map k v -> a
foldlWithKey = Data.Map.Strict.foldlWithKey
