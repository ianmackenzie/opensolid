module Stream
  ( Stream (..)
  , iterate
  , head
  , tail
  , nth
  , map
  , take
  )
where

import OpenSolid
import Prelude qualified

data Stream a = Stream a ~(Stream a)

instance Prelude.Functor Stream where fmap = map

iterate :: a -> (a -> a) -> Stream a
iterate first function = Stream first (iterate (function first) function)

head :: Stream a -> a
head (Stream first _) = first

tail :: Stream a -> Stream a
tail (Stream _ rest) = rest

nth :: Int -> Stream a -> a
nth n stream | n <= 0 = head stream
nth n stream = nth (n - 1) (tail stream)

map :: (a -> b) -> Stream a -> Stream b
map function (Stream first rest) = Stream (function first) (map function rest)

take :: Int -> Stream a -> List a
take n _ | n <= 0 = []
take n stream = head stream : take (n - 1) (tail stream)
