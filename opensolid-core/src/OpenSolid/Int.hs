module OpenSolid.Int
  ( parse
  , pattern Even
  , pattern Odd
  , isEven
  , isOdd
  , sign
  , abs
  , min
  , max
  , clampTo
  , factorial
  , choose
  , sum
  , sumOf
  , product
  , random
  , gcd
  , lcm
  , toInt16
  , fromInt16
  , toInt32
  , fromInt32
  , toInt64
  , fromInt64
  , toWord8
  , fromWord8
  , toWord16
  , fromWord16
  , toWord32
  , fromWord32
  , toWord64
  , fromWord64
  , toCSize
  , fromCSize
  )
where

import Data.Int (Int16, Int32, Int64)
import Data.Ord qualified
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types (CSize)
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude hiding (gcd, lcm, max, min, product, (/))
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

{-# INLINE clampTo #-}
clampTo :: (Int, Int) -> Int -> Int
clampTo = Data.Ord.clamp

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
    | otherwise -> prod (n - d + 1) (n - d + 2) n `div` prod 2 3 d

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

gcd :: Int -> Int -> Int
gcd = Prelude.gcd

lcm :: Int -> Int -> Int
lcm = Prelude.lcm

toInt16 :: Int -> Int16
toInt16 = fromIntegral

fromInt16 :: Int16 -> Int
fromInt16 = fromIntegral

toInt32 :: Int -> Int32
toInt32 = fromIntegral

fromInt32 :: Int32 -> Int
fromInt32 = fromIntegral

toInt64 :: Int -> Int64
toInt64 = fromIntegral

fromInt64 :: Int64 -> Int
fromInt64 = fromIntegral

toWord8 :: Int -> Word8
toWord8 = fromIntegral

fromWord8 :: Word8 -> Int
fromWord8 = fromIntegral

toWord16 :: Int -> Word16
toWord16 = fromIntegral

fromWord16 :: Word16 -> Int
fromWord16 = fromIntegral

toWord32 :: Int -> Word32
toWord32 = fromIntegral

fromWord32 :: Word32 -> Int
fromWord32 = fromIntegral

toWord64 :: Int -> Word64
toWord64 = fromIntegral

fromWord64 :: Word64 -> Int
fromWord64 = fromIntegral

toCSize :: Int -> CSize
toCSize = fromIntegral

fromCSize :: CSize -> Int
fromCSize = fromIntegral
