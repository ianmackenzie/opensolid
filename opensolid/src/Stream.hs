module Stream
  ( Stream (..)
  , repeat
  , iterate
  , head
  , tail
  , nth
  , map
  , mapWithIndex
  , take
  )
where

import OpenSolid
import Prelude qualified

data Stream a = Stream a ~(Stream a)

instance Prelude.Functor Stream where fmap = map

repeat :: a -> Stream a
repeat value = Stream value (repeat value)

iterate :: a -> (a -> a) -> Stream a
iterate first function = Stream first (iterate (function first) function)

head :: Stream a -> a
head (Stream first _) = first

tail :: Stream a -> Stream a
tail (Stream _ rest) = rest

nth :: Int -> Stream a -> a
nth n stream = if n <= 0 then head stream else nth (n - 1) (tail stream)

map :: (a -> b) -> Stream a -> Stream b
map function (Stream first rest) = Stream (function first) (map function rest)

mapWithIndex :: (Int -> a -> b) -> Stream a -> Stream b
mapWithIndex = mapWithIndexImpl 0

mapWithIndexImpl :: Int -> (Int -> a -> b) -> Stream a -> Stream b
mapWithIndexImpl index function (Stream first rest) =
  Stream (function index first) (mapWithIndexImpl (index + 1) function rest)

take :: Int -> Stream a -> List a
take n _ | n <= 0 = []
take n stream = head stream : take (n - 1) (tail stream)
