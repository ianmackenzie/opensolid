module Int
  ( parse
  , pattern Even
  , pattern Odd
  , isEven
  , isOdd
  , sign
  , abs
  , min
  , max
  , factorial
  , choose
  , sum
  , product
  )
where

import List qualified
import NonEmpty qualified
import OpenSolid
import Text.Read qualified
import Prelude qualified

parse :: String -> Result String Int
parse input = Text.Read.readMaybe input ?? Error ("Couldn't parse input as an integer: " + input)

{-# COMPLETE Even, Odd #-}

pattern Even :: Int
pattern Even <- (Prelude.even -> True)

pattern Odd :: Int
pattern Odd <- (Prelude.odd -> True)

isEven :: Int -> Bool
isEven = Prelude.even

isOdd :: Int -> Bool
isOdd = Prelude.odd

sign :: Int -> Sign
sign value = if value >= 0 then Positive else Negative

abs :: Int -> Int
abs = Prelude.abs

min :: Int -> Int -> Int
min = Prelude.min

max :: Int -> Int -> Int
max = Prelude.max

factorial :: Int -> Int
factorial n = prod 1 2 n

choose :: Int -> Int -> Int
choose n k
  | n < 0 = 0
  | k < 0 = 0
  | k > n = 0
  | d == 0 = 1
  | d == 1 = n
  | otherwise = prod (n - d + 1) (n - d + 2) n // prod 2 3 d
 where
  d = min k (n - k)

prod :: Int -> Int -> Int -> Int
prod !acc a b
  | a < b = prod (acc * a) (a + 1) b
  | a == b = acc * a
  | otherwise = acc

sum :: List Int -> Int
sum = List.foldl (+) 0

product :: NonEmpty Int -> Int
product = NonEmpty.reduce (*)
