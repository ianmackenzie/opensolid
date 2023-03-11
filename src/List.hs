module List
  ( isEmpty
  , head
  , map
  , map2
  , map3
  , map4
  , zip
  , zip3
  , zip4
  , filter
  , compact
  , collect
  , combine
  , concat
  , collate
  , collapse
  , foldl
  , foldr
  , reverse
  , drop
  , sum
  , sort
  , sortAndDeduplicate
  , all
  , any
  , (>>=)
  , (>>)
  )
where

import Data.List qualified
import Data.Maybe qualified
import Generic qualified
import OpenSolid hiding ((>>))
import Result qualified
import Prelude qualified

(>>) :: Bool -> List a -> List a
True >> list = list
False >> _ = []

class Bind a b c | a b -> c where
  (>>=) :: a -> b -> c

instance
  a ~ a'
  => Bind
      (List a)
      (a' -> List b)
      (List b)
  where
  (>>=) = (Prelude.>>=)

instance
  a ~ a'
  => Bind
      (Maybe a)
      (a' -> List b)
      (List b)
  where
  Just value >>= function = function value
  Nothing >>= _ = []

instance
  a ~ a'
  => Bind
      (Result x a)
      (a' -> List b)
      (Result x (List b))
  where
  Ok value >>= function = Ok (function value)
  Error error >>= _ = Error error

instance
  (x ~ x', a ~ a')
  => Bind
      (Result x (List a))
      (a' -> List b)
      (Result x (List b))
  where
  Error error >>= _ = Error error
  Ok items >>= function = Ok (combine function items)

instance
  (x ~ x', a ~ a')
  => Bind
      (Result x (List a))
      (a' -> Result x' (List b))
      (Result x (List b))
  where
  Error error >>= _ = Error error
  Ok items >>= function = items >>= function

instance
  a ~ a'
  => Bind
      (List a)
      (a' -> Result x (List b))
      (Result x (List b))
  where
  list >>= function = Result.map concat (collate (map function list))

isEmpty :: List a -> Bool
isEmpty = Prelude.null

data IsEmpty = IsEmpty deriving (Eq, Show)

instance IsError IsEmpty where
  errorMessage IsEmpty = "List is empty"

head :: List a -> Result IsEmpty a
head (first : _) = Ok first
head [] = Error IsEmpty

map :: (a -> b) -> List a -> List b
map = Data.List.map

zip :: List a -> List b -> List (a, b)
zip = Data.List.zip

zip3 :: List a -> List b -> List c -> List (a, b, c)
zip3 = Data.List.zip3

zip4 :: List a -> List b -> List c -> List d -> List (a, b, c, d)
zip4 = Data.List.zip4

map2 :: (a -> b -> c) -> List a -> List b -> List c
map2 = Data.List.zipWith

map3 :: (a -> b -> c -> d) -> List a -> List b -> List c -> List d
map3 = Data.List.zipWith3

map4 :: (a -> b -> c -> d -> e) -> List a -> List b -> List c -> List d -> List e
map4 = Data.List.zipWith4

filter :: (a -> Bool) -> List a -> List a
filter = Data.List.filter

compact :: List (Maybe a) -> List a
compact = Data.Maybe.catMaybes

collect :: (a -> Maybe b) -> List a -> List b
collect = Data.Maybe.mapMaybe

combine :: (a -> List b) -> List a -> List b
combine function list = Prelude.concatMap function list

concat :: List (List a) -> List a
concat = Data.List.concat

collate :: List (Result x a) -> Result x (List a)
collate [] = Ok []
collate (firstResult : remainingResults) = Result.do
  firstItem <- firstResult
  remainingItems <- collate remainingResults
  Ok (firstItem : remainingItems)

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

sortAndDeduplicate :: Ord a => List a -> List a
sortAndDeduplicate list = deduplicate (sort list)

deduplicate :: Eq a => List a -> List a
deduplicate [] = []
deduplicate (first : rest) = dedup first rest

dedup :: Eq a => a -> List a -> List a
dedup current [] = [current]
dedup current (next : remaining) =
  if current == next
    then dedup current remaining
    else current : dedup next remaining

all :: List Bool -> Bool
all = Prelude.and

any :: List Bool -> Bool
any = Prelude.or
