{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module OpenSolid.Domain1d
  ( Domain1d
  , Boundary
  , unit
  , constant
  , endpoints
  , lowerBoundary
  , upperBoundary
  , midpoint
  , width
  , isAtomic
  , bisect
  , half
  , interior
  , bounds
  , includes
  , overlaps
  , contains
  , adjacent
  , intersectionWidth
  , samplingPoints
  , leadingSamplingPoints
  )
where

import OpenSolid.Float qualified as Float
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range

data Domain1d = Domain1d
  { n :: Float
  , i :: Float
  , j :: Float
  }
  deriving (Show)

instance Eq Domain1d where
  Domain1d n1 i1 j1 == Domain1d n2 i2 j2 =
    i1 * n2 == i2 * n1 && j1 * n2 == j2 * n1

data Boundary = Boundary
  { n :: Float
  , i :: Float
  }
  deriving (Show)

instance Eq Boundary where
  Boundary n1 i1 == Boundary n2 i2 =
    i1 * n2 == i2 * n1

instance Ord Boundary where
  compare (Boundary n1 i1) (Boundary n2 i2) =
    compare (i1 * n2) (i2 * n1)

unit :: Domain1d
unit = Domain1d{n = 1.0, i = 0.0, j = 1.0}

constant :: Boundary -> Domain1d
constant Boundary{n, i} = Domain1d{n, i, j = i}

isAtomic :: Domain1d -> Bool
isAtomic (Domain1d{n, i, j}) = (j - i) / n <= Float.epsilon

endpoints :: Domain1d -> (Boundary, Boundary)
endpoints (Domain1d{n, i, j}) = (Boundary{n, i}, Boundary{n, i = j})

lowerBoundary :: Domain1d -> Boundary
lowerBoundary (Domain1d{n, i}) = Boundary{n, i}

upperBoundary :: Domain1d -> Boundary
upperBoundary (Domain1d{n, j}) = Boundary{n, i = j}

midpoint :: Domain1d -> Boundary
midpoint (Domain1d n i j) = Boundary (n * 2.0) (i + j)

width :: Domain1d -> Float
width (Domain1d n i j) = (j - i) / n

bisect :: Domain1d -> (Domain1d, Domain1d)
bisect (Domain1d{n, i, j}) = do
  let n2 = 2.0 * n
  let i2 = 2.0 * i
  let j2 = 2.0 * j
  let mid = i2 + (j - i)
  (Domain1d n2 i2 mid, Domain1d n2 mid j2)

half :: Domain1d -> Domain1d
half (Domain1d{n, i, j}) = do
  let delta = j - i
  Domain1d (4.0 * n) (4.0 * i + delta) (4.0 * j - delta)

bounds :: Domain1d -> Range Unitless
bounds (Domain1d{n, i, j}) = Range.unsafe (i / n) (j / n)

interior :: Domain1d -> Range Unitless
interior (Domain1d{n, i, j}) = do
  let n8 = 8.0 * n
  let delta = j - i
  Range.unsafe
    (if i == 0.0 then 0.0 else (8.0 * i + delta) / n8)
    (if j == n then 1.0 else (8.0 * j - delta) / n8)

overlaps :: Domain1d -> Domain1d -> Bool
overlaps (Domain1d n2 i2 j2) (Domain1d n1 i1 j1) =
  i1 * n2 < j2 * n1 && j1 * n2 > i2 * n1

includes :: Boundary -> Domain1d -> Bool
includes (Boundary{n = pn, i = pi}) (Domain1d{n, i, j}) = do
  pi * n >= i * pn && pi * n <= j * pn

contains :: Domain1d -> Domain1d -> Bool
contains (Domain1d n2 i2 j2) (Domain1d n1 i1 j1) =
  i1 * n2 <= i2 * n1 && j1 * n2 >= j2 * n1

adjacent :: Domain1d -> Domain1d -> Bool
adjacent (Domain1d n1 i1 j1) (Domain1d n2 i2 j2) =
  i1 * n2 == j2 * n1 || j1 * n2 == i2 * n1

intersectionWidth :: Domain1d -> Domain1d -> Float
intersectionWidth (Domain1d n1 i1 j1) (Domain1d n2 i2 j2) = do
  let low = Float.max (i1 / n1) (i2 / n2)
  let high = Float.min (j1 / n1) (j2 / n2)
  Float.max (high - low) 0.0

samplingPoints :: (Range Unitless -> Bool) -> NonEmpty Float
samplingPoints predicate = 0.0 :| collectSamplingPoints predicate Range.unit [1.0]

leadingSamplingPoints :: (Range Unitless -> Bool) -> NonEmpty Float
leadingSamplingPoints predicate = 0.0 :| collectSamplingPoints predicate Range.unit []

collectSamplingPoints :: (Range Unitless -> Bool) -> Range Unitless -> List Float -> List Float
collectSamplingPoints predicate subdomain accumulated = do
  if predicate subdomain
    then accumulated
    else
      if Range.isAtomic subdomain
        then internalError "Infinite recursion in Domain1d.samplingPoints"
        else do
          let (left, right) = Range.bisect subdomain
          let subdomainMidpoint = Range.midpoint subdomain
          let rightAccumulated = collectSamplingPoints predicate right accumulated
          collectSamplingPoints predicate left (subdomainMidpoint : rightAccumulated)
