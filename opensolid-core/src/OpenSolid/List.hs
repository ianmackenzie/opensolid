module OpenSolid.List
  ( List
  , pattern One
  , pattern Two
  , pattern Three
  , pattern Four
  , pattern OneOrMore
  , pattern TwoOrMore
  , pattern ThreeOrMore
  , pattern FourOrMore
  , pattern FiveOrMore
  , singleton
  , maybe
  , isEmpty
  , length
  , map
  , mapWithIndex
  , indexed
  , reverseMap
  , filterMap
  , forEach
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
  , contains
  , indexOf
  , combine
  , concat
  , foldl
  , foldr
  , reverse
  , take
  , drop
  , sort
  , sortBy
  , sortWith
  , sortAndDeduplicate
  , isOrdered
  , isNonDescending
  , isAscending
  , isNonAscending
  , isDescending
  , allSatisfy
  , allTrue
  , anySatisfy
  , anyTrue
  , successive
  , count
  , intersperse
  , partition
  , replicate
  , random
  , shuffle
  )
where

import Data.List qualified
import Data.Maybe qualified
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Random.Internal qualified as Random
import System.Random qualified
import Prelude qualified

singleton :: a -> List a
singleton = Data.List.singleton

maybe :: Maybe a -> List a
maybe (Just value) = [value]
maybe Nothing = []

isEmpty :: List a -> Bool
isEmpty = Prelude.null

length :: List a -> Int
length = Prelude.length

{-# COMPLETE [], OneOrMore #-}

{-# COMPLETE [], One, TwoOrMore #-}

{-# COMPLETE [], One, Two, ThreeOrMore #-}

{-# COMPLETE [], One, Two, Three, FourOrMore #-}

{-# COMPLETE [], One, Two, Three, Four, FiveOrMore #-}

pattern One :: a -> List a
pattern One item = [item]

pattern Two :: a -> a -> List a
pattern Two first second = [first, second]

pattern Three :: a -> a -> a -> List a
pattern Three first second third = [first, second, third]

pattern Four :: a -> a -> a -> a -> List a
pattern Four first second third fourth = [first, second, third, fourth]

pattern OneOrMore :: List a
pattern OneOrMore <- _ : _

pattern TwoOrMore :: List a
pattern TwoOrMore <- _ : _ : _

pattern ThreeOrMore :: List a
pattern ThreeOrMore <- _ : _ : _ : _

pattern FourOrMore :: List a
pattern FourOrMore <- _ : _ : _ : _ : _

pattern FiveOrMore :: a -> a -> a -> a -> a -> List a -> List a
pattern FiveOrMore first second third fourth fifth rest =
  first : second : third : fourth : fifth : rest

map :: (a -> b) -> List a -> List b
map = Data.List.map

mapWithIndex :: (Int -> a -> b) -> List a -> List b
mapWithIndex function list = map2 function [0 ..] list

indexed :: List a -> List (Int, a)
indexed = mapWithIndex (,)

reverseMap :: (a -> b) -> List a -> List b
reverseMap function list = go list []
 where
  go (first : rest) acc = go rest (function first : acc)
  go [] acc = acc

forEach :: List a -> (a -> b) -> List b
forEach list function = map function list

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

filterMap :: (a -> Maybe b) -> List a -> List b
filterMap = Data.Maybe.mapMaybe

find :: (a -> Bool) -> List a -> Maybe a
find = Data.List.find

contains :: Eq a => a -> List a -> Bool
contains = Data.List.elem

indexOf :: Eq a => a -> List a -> Maybe Int
indexOf = Data.List.elemIndex

combine :: (a -> List b) -> List a -> List b
combine = Prelude.foldMap

concat :: List (List a) -> List a
concat = Prelude.concat

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
dedup current (next : remaining)
  | current == next = dedup current remaining
  | otherwise = current : dedup next remaining

isOrdered :: (a -> a -> Bool) -> List a -> Bool
isOrdered _ [] = True
isOrdered cmp (first : rest) = checkOrder cmp first rest

checkOrder :: (a -> a -> Bool) -> a -> List a -> Bool
checkOrder _ _ [] = True
checkOrder cmp current (next : remaining) = cmp current next && checkOrder cmp next remaining

isNonDescending :: Ord a => List a -> Bool
isNonDescending = isOrdered (<=)

isAscending :: Ord a => List a -> Bool
isAscending = isOrdered (<)

isNonAscending :: Ord a => List a -> Bool
isNonAscending = isOrdered (>=)

isDescending :: Ord a => List a -> Bool
isDescending = isOrdered (>)

allSatisfy :: (a -> Bool) -> List a -> Bool
allSatisfy = Prelude.all

allTrue :: List Bool -> Bool
allTrue = Prelude.and

anySatisfy :: (a -> Bool) -> List a -> Bool
anySatisfy = Prelude.any

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

replicate :: Int -> a -> List a
replicate = Data.List.replicate

random :: Int -> Random.Generator a -> Random.Generator (List a)
random n randomItem
  | n <= 0 = Random.return []
  | otherwise = do
      item <- randomItem
      rest <- random (n - 1) randomItem
      Random.return (item : rest)

shuffle :: List a -> Random.Generator (List a)
shuffle original = do
  keys <- random (length original) (Random.Generator System.Random.genWord64)
  let shuffledPairs = sortBy Pair.second (zip2 original keys)
  Random.return (map Pair.first shuffledPairs)
