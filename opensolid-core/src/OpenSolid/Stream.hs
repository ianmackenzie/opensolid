module OpenSolid.Stream
  ( Stream (..)
  , repeat
  , iterate
  , unfold
  , from
  , head
  , tail
  , toList
  , nth
  , drop
  , map
  , mapWithIndex
  , map2
  , take
  )
where

import OpenSolid.Prelude hiding (iterate, repeat, (-))
import Prelude ((+), (-))

data Stream a = Stream a ~(Stream a)

instance Functor Stream where fmap = map

repeat :: a -> Stream a
repeat value = Stream value (repeat value)

iterate :: (a -> a) -> a -> Stream a
iterate function first = Stream first (iterate function (function first))

unfold :: b -> (b -> (a, b)) -> Stream a
unfold initialState function = do
  let (initialValue, updatedState) = function initialState
  Stream initialValue (unfold updatedState function)

from :: Enum a => a -> Stream a
from value = Stream value (from (succ value))

{-# INLINE head #-}
head :: Stream a -> a
head (Stream first _) = first

{-# INLINE tail #-}
tail :: Stream a -> Stream a
tail (Stream _ rest) = rest

toList :: Stream a -> List a
toList (Stream first rest) = first : toList rest

nth :: Int -> Stream a -> a
nth n stream = if n <= 0 then head stream else nth (n - 1) (tail stream)

map :: (a -> b) -> Stream a -> Stream b
map function (Stream first rest) = Stream (function first) (map function rest)

mapWithIndex :: (Int -> a -> b) -> Stream a -> Stream b
mapWithIndex = mapWithIndexImpl 0

mapWithIndexImpl :: Int -> (Int -> a -> b) -> Stream a -> Stream b
mapWithIndexImpl index function (Stream first rest) =
  Stream (function index first) (mapWithIndexImpl (index + 1) function rest)

map2 :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
map2 f (Stream firstA restA) (Stream firstB restB) =
  Stream (f firstA firstB) (map2 f restA restB)

take :: Int -> Stream a -> List a
take n _ | n <= 0 = []
take n stream = head stream : take (n - 1) (tail stream)

drop :: Int -> Stream a -> Stream a
drop n stream | n <= 0 = stream
drop n stream = drop (n - 1) (tail stream)
