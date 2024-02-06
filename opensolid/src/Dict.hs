module Dict
  ( Dict
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
  , foldLeft
  , foldLeftWithKey
  , foldRight
  , foldRightWithKey
  )
where

import Data.Map qualified
import OpenSolid
import Pair qualified
import Prelude qualified

type Dict k v = Data.Map.Map k v

empty :: Dict k v
empty = Data.Map.empty

isEmpty :: Dict k v -> Bool
isEmpty = Data.Map.null

size :: Dict k v -> Int
size = Data.Map.size

singleton :: k -> v -> Dict k v
singleton = Data.Map.singleton

fromList :: Ord k => List (k, v) -> Dict k v
fromList = Data.Map.fromList

toList :: Dict k v -> List (k, v)
toList = Data.Map.toList

get :: Ord k => k -> Dict k v -> Maybe v
get = Data.Map.lookup

take :: Ord k => k -> Dict k v -> (Maybe v, Dict k v)
take = Data.Map.updateLookupWithKey (\_ _ -> Nothing)

set :: Ord k => k -> v -> Dict k v -> Dict k v
set = Data.Map.insert

update :: Ord k => (Maybe v -> Maybe v) -> k -> Dict k v -> Dict k v
update = Data.Map.alter

remove :: Ord k => k -> Dict k v -> Dict k v
remove = Data.Map.delete

keys :: Dict k v -> List k
keys = Data.Map.keys

values :: Dict k v -> List v
values = Data.Map.elems

filter :: (v -> Bool) -> Dict k v -> Dict k v
filter = Data.Map.filter

filterWithKey :: (k -> v -> Bool) -> Data.Map.Map k v -> Data.Map.Map k v
filterWithKey = Data.Map.filterWithKey

map :: (v -> w) -> Dict k v -> Dict k w
map = Prelude.fmap

mapWithKey :: (k -> v -> w) -> Dict k v -> Dict k w
mapWithKey = Data.Map.mapWithKey

merge :: Ord k => (v -> v -> v) -> Dict k v -> Dict k v -> Dict k v
merge = Data.Map.unionWith

mergeWithKey :: Ord k => (k -> v -> v -> v) -> Dict k v -> Dict k v -> Dict k v
mergeWithKey = Data.Map.unionWithKey

getMin :: Ord k => Dict k v -> Maybe (k, v)
getMin dict = if isEmpty dict then Nothing else Just (Data.Map.findMin dict)

getMax :: Ord k => Dict k v -> Maybe (k, v)
getMax dict = if isEmpty dict then Nothing else Just (Data.Map.findMax dict)

takeMin :: Ord k => Dict k v -> (Maybe (k, v), Dict k v)
takeMin dict
  | isEmpty dict = (Nothing, dict)
  | otherwise = Pair.mapFirst Just (Data.Map.deleteFindMin dict)

takeMax :: Ord k => Dict k v -> (Maybe (k, v), Dict k v)
takeMax dict
  | isEmpty dict = (Nothing, dict)
  | otherwise = Pair.mapFirst Just (Data.Map.deleteFindMax dict)

foldRight :: (v -> a -> a) -> a -> Dict k v -> a
foldRight = Data.Map.foldr

foldRightWithKey :: (k -> v -> a -> a) -> a -> Dict k v -> a
foldRightWithKey = Data.Map.foldrWithKey

foldLeft :: (a -> v -> a) -> a -> Dict k v -> a
foldLeft = Data.Map.foldl'

foldLeftWithKey :: (a -> k -> v -> a) -> a -> Dict k v -> a
foldLeftWithKey = Data.Map.foldlWithKey
