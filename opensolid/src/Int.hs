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
  , sumOf
  , product
  , random
  )
where

import List qualified
import NonEmpty qualified
import OpenSolid.Prelude
import OpenSolid.Random.Internal (Generator (Generator))
import OpenSolid.Text.Parse qualified as Text.Parse
import System.Random qualified
import Prelude qualified

parse :: Text -> Result Text Int
parse = Text.Parse.int

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
choose n k = do
  let d = min k (n - k)
  if
    | n < 0 -> 0
    | k < 0 -> 0
    | k > n -> 0
    | d == 0 -> 1
    | d == 1 -> n
    | otherwise -> prod (n - d + 1) (n - d + 2) n // prod 2 3 d

prod :: Int -> Int -> Int -> Int
prod !acc a b = case compare a b of
  LT -> prod (acc * a) (a + 1) b
  EQ -> acc * a
  GT -> acc

sum :: List Int -> Int
sum = List.foldl (+) 0

sumOf :: (a -> Int) -> List a -> Int
sumOf f list = sum (List.map f list)

product :: NonEmpty Int -> Int
product = NonEmpty.reduce (*)

random :: Int -> Int -> Generator Int
random low high = Generator (System.Random.uniformR (low, high))
