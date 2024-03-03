module NonEmpty
  ( NonEmpty ((:|))
  , pattern NonEmpty
  , (|:)
  , singleton
  , range
  , of2
  , of3
  , of4
  , of5
  , of6
  , of7
  , of8
  , first
  , rest
  , last
  , toList
  , prepend
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
  , concat
  , foldLeft
  , foldRight
  , reduceLeft
  , reduceRight
  , reverse
  , take
  , drop
  , sum
  , sumOf
  , sort
  , sortBy
  , sortWith
  , sortAndDeduplicate
  , intersperse
  , partition
  , all
  , allTrue
  , any
  , anyTrue
  , minimum
  , maximum
  , minimumOf
  , maximumOf
  , minimumBy
  , maximumBy
  , pickMinimum
  , pickMinimumBy
  , pickMaximum
  , pickMaximumBy
  )
where

import Arithmetic
import Basics
import Data.Foldable qualified
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified
import Data.Semigroup qualified
import List qualified
import Prelude qualified

{-# COMPLETE [], NonEmpty #-}

pattern NonEmpty :: NonEmpty a -> List a
pattern NonEmpty nonEmpty <- (Data.List.NonEmpty.nonEmpty -> Just nonEmpty)

(|:) :: List a -> a -> NonEmpty a
[] |: item = singleton item
(x : xs) |: item = x :| Prelude.mappend xs [item]

singleton :: a -> NonEmpty a
singleton value = value :| []

range :: Int -> Int -> NonEmpty Int
range a b
  | a < b = a :| List.range (a + 1) b
  | b < a = a :| List.range (a - 1) b
  | otherwise = a :| []

of2 :: a -> a -> NonEmpty a
of2 a1 a2 = a1 :| [a2]

of3 :: a -> a -> a -> NonEmpty a
of3 a1 a2 a3 = a1 :| [a2, a3]

of4 :: a -> a -> a -> a -> NonEmpty a
of4 a1 a2 a3 a4 = a1 :| [a2, a3, a4]

of5 :: a -> a -> a -> a -> a -> NonEmpty a
of5 a1 a2 a3 a4 a5 = a1 :| [a2, a3, a4, a5]

of6 :: a -> a -> a -> a -> a -> a -> NonEmpty a
of6 a1 a2 a3 a4 a5 a6 = a1 :| [a2, a3, a4, a5, a6]

of7 :: a -> a -> a -> a -> a -> a -> a -> NonEmpty a
of7 a1 a2 a3 a4 a5 a6 a7 = a1 :| [a2, a3, a4, a5, a6, a7]

of8 :: a -> a -> a -> a -> a -> a -> a -> a -> NonEmpty a
of8 a1 a2 a3 a4 a5 a6 a7 a8 = a1 :| [a2, a3, a4, a5, a6, a7, a8]

first :: NonEmpty a -> a
first = Data.List.NonEmpty.head

rest :: NonEmpty a -> List a
rest = Data.List.NonEmpty.tail

last :: NonEmpty a -> a
last = Data.List.NonEmpty.last

toList :: NonEmpty a -> List a
toList (x :| xs) = x : xs

prepend :: a -> NonEmpty a -> NonEmpty a
prepend = Data.List.NonEmpty.cons

length :: NonEmpty a -> Int
length = Data.List.NonEmpty.length

map :: (a -> b) -> NonEmpty a -> NonEmpty b
map = Data.List.NonEmpty.map

mapWithIndex :: (Int -> a -> b) -> NonEmpty a -> NonEmpty b
mapWithIndex function (x :| xs) = function 0 x :| List.map2 function [1 ..] xs

reverseMap :: (a -> b) -> NonEmpty a -> NonEmpty b
reverseMap function (x :| xs) = go x xs []
 where
  go item [] acc = function item :| acc
  go item (next : following) acc = go next following (function item : acc)

map2 :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
map2 = Data.List.NonEmpty.zipWith

map3 :: (a -> b -> c -> d) -> NonEmpty a -> NonEmpty b -> NonEmpty c -> NonEmpty d
map3 function (a :| as) (b :| bs) (c :| cs) =
  function a b c :| List.map3 function as bs cs

map4 :: (a -> b -> c -> d -> e) -> NonEmpty a -> NonEmpty b -> NonEmpty c -> NonEmpty d -> NonEmpty e
map4 function (a :| as) (b :| bs) (c :| cs) (d :| ds) =
  function a b c d :| List.map4 function as bs cs ds

zip2 :: NonEmpty a -> NonEmpty b -> NonEmpty (a, b)
zip2 = Data.List.NonEmpty.zip

zip3 :: NonEmpty a -> NonEmpty b -> NonEmpty c -> NonEmpty (a, b, c)
zip3 (a :| as) (b :| bs) (c :| cs) =
  (a, b, c) :| List.zip3 as bs cs

zip4 :: NonEmpty a -> NonEmpty b -> NonEmpty c -> NonEmpty d -> NonEmpty (a, b, c, d)
zip4 (a :| as) (b :| bs) (c :| cs) (d :| ds) =
  (a, b, c, d) :| List.zip4 as bs cs ds

unzip2 :: NonEmpty (a, b) -> (NonEmpty a, NonEmpty b)
unzip2 = Data.List.NonEmpty.unzip

unzip3 :: NonEmpty (a, b, c) -> (NonEmpty a, NonEmpty b, NonEmpty c)
unzip3 nonEmpty =
  let (a, b, c) = first nonEmpty
      (as, bs, cs) = List.unzip3 (rest nonEmpty)
   in (a :| as, b :| bs, c :| cs)

unzip4 :: NonEmpty (a, b, c, d) -> (NonEmpty a, NonEmpty b, NonEmpty c, NonEmpty d)
unzip4 nonEmpty =
  let (a, b, c, d) = first nonEmpty
      (as, bs, cs, ds) = List.unzip4 (rest nonEmpty)
   in (a :| as, b :| bs, c :| cs, d :| ds)

filter :: (a -> Bool) -> NonEmpty a -> List a
filter = Data.List.NonEmpty.filter

find :: (a -> Bool) -> NonEmpty a -> Maybe a
find = Data.Foldable.find

concat :: NonEmpty (NonEmpty a) -> NonEmpty a
concat = Data.Semigroup.sconcat

foldLeft :: (b -> a -> b) -> b -> NonEmpty a -> b
foldLeft = Data.Foldable.foldl'

foldRight :: (a -> b -> b) -> b -> NonEmpty a -> b
foldRight = Data.Foldable.foldr

reduceLeft :: (a -> a -> a) -> NonEmpty a -> a
reduceLeft function (x :| xs) = List.foldLeft function x xs

reduceRight :: (a -> a -> a) -> NonEmpty a -> a
reduceRight = Data.Foldable.foldr1

reverse :: NonEmpty a -> NonEmpty a
reverse = Data.List.NonEmpty.reverse

take :: Int -> NonEmpty a -> List a
take = Data.List.NonEmpty.take

drop :: Int -> NonEmpty a -> List a
drop = Data.List.NonEmpty.drop

sum :: Addition a a a => NonEmpty a -> a
sum = reduceLeft (+)

sumOf :: Addition b b b => (a -> b) -> NonEmpty a -> b
sumOf function nonEmpty = sum (map function nonEmpty)

sort :: Ord a => NonEmpty a -> NonEmpty a
sort = Data.List.NonEmpty.sort

sortBy :: Ord b => (a -> b) -> NonEmpty a -> NonEmpty a
sortBy property = sortWith (\a b -> compare (property a) (property b))

sortWith :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a
sortWith = Data.List.NonEmpty.sortBy

sortAndDeduplicate :: Ord a => NonEmpty a -> NonEmpty a
sortAndDeduplicate nonEmpty = deduplicate (sort nonEmpty)

deduplicate :: Eq a => NonEmpty a -> NonEmpty a
deduplicate nonEmpty = dedup (first nonEmpty) (rest nonEmpty)

dedup :: Eq a => a -> List a -> NonEmpty a
dedup current [] = current :| []
dedup current (next : remaining)
  | current == next = dedup current remaining
  | otherwise = prepend current (dedup next remaining)

all :: (a -> Bool) -> NonEmpty a -> Bool
all = Prelude.all

allTrue :: NonEmpty Bool -> Bool
allTrue = Prelude.and

any :: (a -> Bool) -> NonEmpty a -> Bool
any = Prelude.any

anyTrue :: NonEmpty Bool -> Bool
anyTrue = Prelude.or

intersperse :: a -> NonEmpty a -> NonEmpty a
intersperse = Data.List.NonEmpty.intersperse

partition :: (a -> Bool) -> NonEmpty a -> (List a, List a)
partition = Data.List.NonEmpty.partition

minimum :: Ord a => NonEmpty a -> a
minimum = Prelude.minimum

maximum :: Ord a => NonEmpty a -> a
maximum = Prelude.maximum

minimumOf :: Ord b => (a -> b) -> NonEmpty a -> b
minimumOf property nonEmpty = minimum (map property nonEmpty)

maximumOf :: Ord b => (a -> b) -> NonEmpty a -> b
maximumOf property nonEmpty = maximum (map property nonEmpty)

minimumBy :: Ord b => (a -> b) -> NonEmpty a -> a
minimumBy property (x :| xs) =
  extremum (<) property x (property x) xs

maximumBy :: Ord b => (a -> b) -> NonEmpty a -> a
maximumBy property (x :| xs) =
  extremum (>) property x (property x) xs

extremum :: (b -> b -> Bool) -> (a -> b) -> a -> b -> List a -> a
extremum _ _ current _ [] = current
extremum comparison property current currentProperty (next : remaining) =
  let nextProperty = property next
   in if comparison nextProperty currentProperty
        then extremum comparison property next nextProperty remaining
        else extremum comparison property current currentProperty remaining

prependReversed :: List a -> List a -> List a
prependReversed [] list = list
prependReversed (x : xs) list = prependReversed xs (x : list)

pickMinimum :: Ord a => NonEmpty a -> (a, List a)
pickMinimum = pick (<)

pickMinimumBy :: Ord b => (a -> b) -> NonEmpty a -> (a, List a)
pickMinimumBy property = pick (\item1 item2 -> property item1 < property item2)

pickMaximum :: Ord a => NonEmpty a -> (a, List a)
pickMaximum = pick (>)

pickMaximumBy :: Ord b => (a -> b) -> NonEmpty a -> (a, List a)
pickMaximumBy property = pick (\item1 item2 -> property item1 > property item2)

pick :: (a -> a -> Bool) -> NonEmpty a -> (a, List a)
pick better (x :| xs) = go [x] [] x xs xs
 where
  go previous currentPrevious currentItem currentFollowing following =
    case following of
      [] -> (currentItem, prependReversed currentPrevious currentFollowing)
      item : remaining ->
        let updatedPrevious = item : previous
         in if better item currentItem
              then go updatedPrevious previous item remaining remaining
              else go updatedPrevious currentPrevious currentItem currentFollowing remaining
