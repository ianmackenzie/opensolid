module NonEmpty
  ( NonEmpty ((:|))
  , pattern NonEmpty
  , singleton
  , first
  , rest
  , toList
  , prepend
  , length
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
  , any
  , minimum
  , maximum
  , minimumOf
  , maximumOf
  , minimumBy
  , maximumBy
  )
where

import Arithmetic
import Basics
import Data.Foldable qualified
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified
import Generic (Ordering)
import Generic qualified
import Data.Semigroup qualified
import List qualified
import Prelude qualified

{-# COMPLETE [], NonEmpty #-}

pattern NonEmpty :: NonEmpty a -> List a
pattern NonEmpty nonEmpty <- (Data.List.NonEmpty.nonEmpty -> Just nonEmpty)

singleton :: a -> NonEmpty a
singleton value = value :| []

first :: NonEmpty a -> a
first = Data.List.NonEmpty.head

rest :: NonEmpty a -> List a
rest = Data.List.NonEmpty.tail

toList :: NonEmpty a -> List a
toList (x :| xs) = x : xs

prepend :: a -> NonEmpty a -> NonEmpty a
prepend = Data.List.NonEmpty.cons

length :: NonEmpty a -> Int
length = Data.List.NonEmpty.length

map :: (a -> b) -> NonEmpty a -> NonEmpty b
map = Data.List.NonEmpty.map

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
sortBy property = sortWith (\a b -> Generic.compare (property a) (property b))

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

any :: (a -> Bool) -> NonEmpty a -> Bool
any = Prelude.any

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
