{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Solve1d
  ( Subdomain
  , domain
  , init
  , IsAtomic (IsAtomic)
  , enqueueChildren
  , isAtomic
  , bisect
  , half
  , interior
  , bounds
  , overlaps
  , contains
  , isResolved
  , resolvedSign
  , Neighborhood
  , neighborhood
  , derivativeTolerance
  )
where

import Float qualified
import Int qualified
import OpenSolid
import Qty qualified
import Queue (Queue)
import Queue qualified
import Range (Range)
import Range qualified

data Subdomain = Subdomain
  { n :: Float
  , i :: Float
  , j :: Float
  }
  deriving (Eq, Show)

domain :: Subdomain
domain = Subdomain{n = 1.0, i = 0.0, j = 1.0}

init :: Queue Subdomain
init = Queue.singleton domain

isAtomic :: Subdomain -> Bool
isAtomic (Subdomain{n, i, j}) = (j - i) / n <= Float.epsilon

bisect :: Subdomain -> (Subdomain, Subdomain)
bisect (Subdomain{n, i, j}) = do
  let n2 = 2 * n
  let i2 = 2 * i
  let j2 = 2 * j
  let mid = i2 + (j - i)
  (Subdomain n2 i2 mid, Subdomain n2 mid j2)

half :: Subdomain -> Subdomain
half (Subdomain{n, i, j}) = do
  let delta = j - i
  Subdomain (4 * n) (4 * i + delta) (4 * j - delta)

bounds :: Subdomain -> Range Unitless
bounds (Subdomain{n, i, j}) = Range.unsafe (i / n) (j / n)

interior :: Subdomain -> Range Unitless
interior (Subdomain{n, i, j}) = do
  let n8 = 8 * n
  let delta = j - i
  Range.unsafe
    (if i == 0.0 then 0.0 else (8 * i + delta) / n8)
    (if j == n then 1.0 else (8 * j - delta) / n8)

overlaps :: Subdomain -> Subdomain -> Bool
overlaps (Subdomain n2 i2 j2) (Subdomain n1 i1 j1) =
  i1 * n2 < j2 * n1 && j1 * n2 > i2 * n1

contains :: Subdomain -> Subdomain -> Bool
contains (Subdomain n2 i2 j2) (Subdomain n1 i1 j1) =
  i1 * n2 <= i2 * n1 && j1 * n2 >= j2 * n1

isResolved :: Range units -> Bool
isResolved range = resolvedSign range /= Nothing

resolvedSign :: Range units -> Maybe Sign
resolvedSign range = do
  let resolution = Range.resolution range
  if Qty.abs resolution >= 0.5 then Just (Qty.sign resolution) else Nothing

data IsAtomic = IsAtomic deriving (Eq, Show, Error)

enqueueChildren :: Subdomain -> Queue Subdomain -> Result IsAtomic (Queue Subdomain)
enqueueChildren subdomain queue
  | isAtomic subdomain = Error IsAtomic
  | otherwise = do
      let mid = half subdomain
      let (left, right) = bisect subdomain
      Ok (queue |> Queue.push mid |> Queue.push left |> Queue.push right)

data Neighborhood units = Neighborhood {n :: Int, derivativeMagnitude :: Qty units, radius :: Float}

neighborhood :: Tolerance units => Int -> Qty units -> Neighborhood units
neighborhood n derivativeMagnitude = do
  let radius = (Int.factorial n * ?tolerance / derivativeMagnitude) ** (1 / n)
  Neighborhood{n, derivativeMagnitude, radius}

derivativeTolerance :: Neighborhood units -> Int -> Qty units
derivativeTolerance (Neighborhood{n, derivativeMagnitude, radius}) k =
  derivativeMagnitude * radius ** (n - k) / Int.factorial (n - k)
