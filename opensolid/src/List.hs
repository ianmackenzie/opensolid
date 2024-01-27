module List
  ( List
  , pattern One
  , pattern Two
  , pattern Three
  , pattern TwoOrMore
  , pattern ThreeOrMore
  , pattern FourOrMore
  , singleton
  , isEmpty
  , length
  , map
  , mapWithIndex
  , reverseMap
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
  , find
  , collect
  , concat
  , foldLeft
  , foldRight
  , reverse
  , take
  , drop
  , sum
  , sumOf
  , sort
  , sortBy
  , sortWith
  , sortAndDeduplicate
  , all
  , allTrue
  , any
  , anyTrue
  , successive
  , count
  , intersperse
  , partition
  , repeat
  )
where

import Arithmetic
import Basics
import Data.List qualified
import Generic qualified
import Prelude qualified

singleton :: a -> List a
singleton = Data.List.singleton

isEmpty :: List a -> Bool
isEmpty = Prelude.null

length :: List a -> Int
length = Data.List.length

{-# COMPLETE [], One, TwoOrMore #-}

{-# COMPLETE [], One, Two, ThreeOrMore #-}

{-# COMPLETE [], One, Two, Three, FourOrMore #-}

pattern One :: a -> List a
pattern One item = [item]

pattern Two :: a -> a -> List a
pattern Two first second = [first, second]

pattern Three :: a -> a -> a -> List a
pattern Three first second third = [first, second, third]

pattern TwoOrMore :: List a
pattern TwoOrMore <- _ : _ : _

pattern ThreeOrMore :: List a
pattern ThreeOrMore <- _ : _ : _ : _

pattern FourOrMore :: List a
pattern FourOrMore <- _ : _ : _ : _ : _

map :: (a -> b) -> List a -> List b
map = Data.List.map

mapWithIndex :: (Int -> a -> b) -> List a -> List b
mapWithIndex fn = map2 fn [0 ..]

reverseMap :: (a -> b) -> List a -> List b
reverseMap function list = go list []
 where
  go (first : rest) acc = go rest (function first : acc)
  go [] acc = acc

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

find :: (a -> Bool) -> List a -> Maybe a
find = Data.List.find

collect :: (a -> List b) -> List a -> List b
collect = Prelude.concatMap

concat :: List (List a) -> List a
concat = Data.List.concat

foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft = Data.List.foldl'

foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight = Data.List.foldr

reverse :: List a -> List a
reverse = Data.List.reverse

take :: Int -> List a -> List a
take = Prelude.take

drop :: Int -> List a -> List a
drop = Prelude.drop

sum :: (Addition a a a) => List a -> a
sum = foldLeft (+) Generic.zero

sumOf :: (Addition b b b) => (a -> b) -> List a -> b
sumOf function list = sum (map function list)

sort :: (Ord a) => List a -> List a
sort = Data.List.sort

sortBy :: (Ord b) => (a -> b) -> List a -> List a
sortBy = Data.List.sortOn

sortWith :: (a -> a -> Ordering) -> List a -> List a
sortWith = Data.List.sortBy

sortAndDeduplicate :: (Ord a) => List a -> List a
sortAndDeduplicate list = deduplicate (sort list)

deduplicate :: (Eq a) => List a -> List a
deduplicate [] = []
deduplicate (first : rest) = dedup first rest

dedup :: (Eq a) => a -> List a -> List a
dedup current [] = [current]
dedup current (next : remaining)
  | current == next = dedup current remaining
  | otherwise = current : dedup next remaining

all :: (a -> Bool) -> List a -> Bool
all = Prelude.all

allTrue :: List Bool -> Bool
allTrue = Prelude.and

any :: (a -> Bool) -> List a -> Bool
any = Prelude.any

anyTrue :: List Bool -> Bool
anyTrue = Prelude.or

successive :: (a -> a -> b) -> List a -> List b
successive function list = map2 function list (drop 1 list)

count :: (a -> Bool) -> List a -> Int
count _ [] = 0
count predicate (first : rest) = (if predicate first then 1 else 0) + count predicate rest

intersperse :: a -> List a -> List a
intersperse = Data.List.intersperse

partition :: (a -> Bool) -> List a -> (List a, List a)
partition = Data.List.partition

repeat :: Int -> a -> List a
repeat = Data.List.replicate
