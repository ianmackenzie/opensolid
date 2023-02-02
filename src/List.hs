module List
  ( isEmpty
  , head
  , map
  , filter
  , compact
  , collect
  , combine
  , concat
  , collapse
  , foldl
  , foldr
  , reverse
  , drop
  , sum
  , sort
  )
where

import Data.List qualified
import Data.Maybe qualified
import Generic qualified
import OpenSolid
import Prelude qualified

isEmpty :: List a -> Bool
isEmpty = Prelude.null

head :: List a -> Maybe a
head (first : _) = Just first
head [] = Nothing

map :: (a -> b) -> List a -> List b
map = Data.List.map

filter :: (a -> Bool) -> List a -> List a
filter = Data.List.filter

compact :: List (Maybe a) -> List a
compact = Data.Maybe.catMaybes

collect :: (a -> Maybe b) -> List a -> List b
collect = Data.Maybe.mapMaybe

combine :: (a -> List b) -> List a -> List b
combine function list = list >>= function

concat :: List (List a) -> List a
concat = Data.List.concat

collapse :: (a -> a -> Maybe a) -> List a -> List a
collapse _ [] = []
collapse function (first : rest) = go first rest
 where
  go current [] = [current]
  go current (next : remaining) =
    case function current next of
      Just merged -> go merged remaining
      Nothing -> current : go next remaining

foldl :: (b -> a -> b) -> b -> List a -> b
foldl = Data.List.foldl'

foldr :: (a -> b -> b) -> b -> List a -> b
foldr = Data.List.foldr

reverse :: List a -> List a
reverse = Data.List.reverse

drop :: Int -> List a -> List a
drop = Prelude.drop

sum :: (Generic.Zero a, Addition a a a) => List a -> a
sum = foldl (+) Generic.zero

sort :: Ord a => List a -> List a
sort = Data.List.sort
