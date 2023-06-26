module List
  ( List
  , isEmpty
  , length
  , head
  , map
  , map2
  , map3
  , map4
  , zip2
  , zip3
  , zip4
  , unzip2
  , unzip3
  , unzip4
  , filter
  , collect
  , concat
  , collapse
  , foldl
  , foldr
  , reverse
  , take
  , drop
  , sum
  , sort
  , sortBy
  , sortWith
  , sortAndDeduplicate
  , all
  , any
  , successive
  , count
  , intersperse
  )
where

import Arithmetic
import Basics
import Data.List qualified
import Generic qualified
import Result (ErrorMessage, Result (..))
import Prelude qualified

isEmpty :: List a -> Bool
isEmpty = Prelude.null

length :: List a -> Int
length = Data.List.length

data IsEmpty = IsEmpty deriving (Eq, Show, ErrorMessage)

head :: List a -> Result IsEmpty a
head (first : _) = Ok first
head [] = Error IsEmpty

map :: (a -> b) -> List a -> List b
map = Data.List.map

zip2 :: List a -> List b -> List (a, b)
zip2 = Data.List.zip

zip3 :: List a -> List b -> List c -> List (a, b, c)
zip3 = Data.List.zip3

zip4 :: List a -> List b -> List c -> List d -> List (a, b, c, d)
zip4 = Data.List.zip4

unzip2 :: List (a, b) -> (List a, List b)
unzip2 = Data.List.unzip

unzip3 :: List (a, b, c) -> (List a, List b, List c)
unzip3 = Data.List.unzip3

unzip4 :: List (a, b, c, d) -> (List a, List b, List c, List d)
unzip4 = Data.List.unzip4

map2 :: (a -> b -> c) -> List a -> List b -> List c
map2 = Data.List.zipWith

map3 :: (a -> b -> c -> d) -> List a -> List b -> List c -> List d
map3 = Data.List.zipWith3

map4 :: (a -> b -> c -> d -> e) -> List a -> List b -> List c -> List d -> List e
map4 = Data.List.zipWith4

filter :: (a -> Bool) -> List a -> List a
filter = Data.List.filter

collect :: (a -> List b) -> List a -> List b
collect = Prelude.concatMap

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

take :: Int -> List a -> List a
take = Prelude.take

drop :: Int -> List a -> List a
drop = Prelude.drop

sum :: (Generic.Zero a, Addition a a a) => List a -> a
sum = foldl (+) Generic.zero

sort :: Ord a => List a -> List a
sort = Data.List.sort

sortBy :: Ord b => (a -> b) -> List a -> List a
sortBy = Data.List.sortOn

sortWith :: (a -> a -> Ordering) -> List a -> List a
sortWith = Data.List.sortBy

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

all :: (a -> Bool) -> List a -> Bool
all = Prelude.all

any :: (a -> Bool) -> List a -> Bool
any = Prelude.any

successive :: (a -> a -> b) -> List a -> List b
successive function list = map2 function list (drop 1 list)

count :: (a -> Bool) -> List a -> Int
count _ [] = 0
count predicate (first : rest) = (if predicate first then 1 else 0) + count predicate rest

intersperse :: a -> List a -> List a
intersperse = Data.List.intersperse
