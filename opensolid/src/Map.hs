module Map
  ( Map
  , empty
  , isEmpty
  , size
  , singleton
  , fromList
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

import Basics
import Data.Map (Map)
import Data.Map qualified
import Pair qualified
import Prelude qualified

empty :: Map k v
empty = Data.Map.empty

isEmpty :: Map k v -> Bool
isEmpty = Data.Map.null

size :: Map k v -> Int
size = Data.Map.size

singleton :: k -> v -> Map k v
singleton = Data.Map.singleton

fromList :: Ord k => List (k, v) -> Map k v
fromList = Data.Map.fromList

toList :: Map k v -> List (k, v)
toList = Data.Map.toList

get :: Ord k => k -> Map k v -> Maybe v
get = Data.Map.lookup

take :: Ord k => k -> Map k v -> (Maybe v, Map k v)
take = Data.Map.updateLookupWithKey (\_ _ -> Nothing)

set :: Ord k => k -> v -> Map k v -> Map k v
set = Data.Map.insert

update :: Ord k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
update = Data.Map.alter

remove :: Ord k => k -> Map k v -> Map k v
remove = Data.Map.delete

keys :: Map k v -> List k
keys = Data.Map.keys

values :: Map k v -> List v
values = Data.Map.elems

filter :: (v -> Bool) -> Map k v -> Map k v
filter = Data.Map.filter

filterWithKey :: (k -> v -> Bool) -> Data.Map.Map k v -> Data.Map.Map k v
filterWithKey = Data.Map.filterWithKey

map :: (v -> w) -> Map k v -> Map k w
map = Prelude.fmap

mapWithKey :: (k -> v -> w) -> Map k v -> Map k w
mapWithKey = Data.Map.mapWithKey

merge :: Ord k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
merge = Data.Map.unionWith

mergeWithKey :: Ord k => (k -> v -> v -> v) -> Map k v -> Map k v -> Map k v
mergeWithKey = Data.Map.unionWithKey

getMin :: Ord k => Map k v -> Maybe (k, v)
getMin givenMap = if isEmpty givenMap then Nothing else Just (Data.Map.findMin givenMap)

getMax :: Ord k => Map k v -> Maybe (k, v)
getMax givenMap = if isEmpty givenMap then Nothing else Just (Data.Map.findMax givenMap)

takeMin :: Ord k => Map k v -> (Maybe (k, v), Map k v)
takeMin givenMap =
  if isEmpty givenMap
    then (Nothing, givenMap)
    else Pair.mapFirst Just (Data.Map.deleteFindMin givenMap)

takeMax :: Ord k => Map k v -> (Maybe (k, v), Map k v)
takeMax givenMap =
  if isEmpty givenMap
    then (Nothing, givenMap)
    else Pair.mapFirst Just (Data.Map.deleteFindMax givenMap)

foldr :: (v -> a -> a) -> a -> Map k v -> a
foldr = Data.Map.foldr

foldrWithKey :: (k -> v -> a -> a) -> a -> Map k v -> a
foldrWithKey = Data.Map.foldrWithKey

foldl :: (a -> v -> a) -> a -> Map k v -> a
foldl = Data.Map.foldl'

foldlWithKey :: (a -> k -> v -> a) -> a -> Map k v -> a
foldlWithKey = Data.Map.foldlWithKey
