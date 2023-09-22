module Seq
  ( Seq
  , pattern Empty
  , pattern (:<)
  , pattern (:>)
  , empty
  , fromList
  , toList
  , isEmpty
  , concat
  , map
  , filter
  , foldLeft
  , foldRight
  , reverse
  , sort
  , sortBy
  , sortWith
  , all
  , any
  )
where

import Data.Foldable qualified
import Data.Sequence qualified
import Generic (Ordering)
import OpenSolid
import Prelude qualified

type Seq a = Data.Sequence.Seq a

pattern Empty :: Seq a
pattern Empty = Data.Sequence.Empty

{-# COMPLETE Empty, (:<) #-}

pattern (:<) :: a -> Seq a -> Seq a
pattern item :< seq = item Data.Sequence.:<| seq

{-# COMPLETE Empty, (:>) #-}

pattern (:>) :: Seq a -> a -> Seq a
pattern seq :> item = seq Data.Sequence.:|> item

empty :: Seq a
empty = Data.Sequence.empty

fromList :: List a -> Seq a
fromList = Data.Sequence.fromList

toList :: Seq a -> List a
toList = Data.Foldable.toList

isEmpty :: Seq a -> Bool
isEmpty = Data.Sequence.null

concat :: Seq a -> Seq a -> Seq a
concat = (Data.Sequence.><)

map :: (a -> b) -> Seq a -> Seq b
map = Prelude.fmap

filter :: (a -> Bool) -> Seq a -> Seq a
filter = Data.Sequence.filter

foldLeft :: (b -> a -> b) -> b -> Seq a -> b
foldLeft = Data.Foldable.foldl'

foldRight :: (a -> b -> b) -> b -> Seq a -> b
foldRight = Data.Foldable.foldr

reverse :: Seq a -> Seq a
reverse = Data.Sequence.reverse

sort :: (Ord a) => Seq a -> Seq a
sort = Data.Sequence.sort

sortBy :: (Ord b) => (a -> b) -> Seq a -> Seq a
sortBy = Data.Sequence.sortOn

sortWith :: (a -> a -> Ordering) -> Seq a -> Seq a
sortWith = Data.Sequence.sortBy

all :: (a -> Bool) -> Seq a -> Bool
all = Prelude.all

any :: (a -> Bool) -> Seq a -> Bool
any = Prelude.any
