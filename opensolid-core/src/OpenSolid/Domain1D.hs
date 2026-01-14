module OpenSolid.Domain1D
  ( Domain1D
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
  , innerSamplingPoints
  , leadingSamplingPoints
  , trailingSamplingPoints
  )
where

import GHC.Records (HasField (getField))
import OpenSolid.InternalError (InternalError (InternalError))
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude

data Domain1D = Domain1D
  { n :: Number
  , i :: Number
  , j :: Number
  }
  deriving (Show)

instance HasField "bounds" Domain1D (Interval Unitless) where
  getField = bounds

instance Eq Domain1D where
  Domain1D n1 i1 j1 == Domain1D n2 i2 j2 =
    i1 .*. n2 == i2 .*. n1 && j1 .*. n2 == j2 .*. n1

data Boundary = Boundary
  { n :: Number
  , i :: Number
  }
  deriving (Show)

instance Eq Boundary where
  Boundary n1 i1 == Boundary n2 i2 =
    i1 .*. n2 == i2 .*. n1

instance Ord Boundary where
  compare (Boundary n1 i1) (Boundary n2 i2) =
    compare (i1 .*. n2) (i2 .*. n1)

unit :: Domain1D
unit = Domain1D{n = 1, i = 0, j = 1}

constant :: Boundary -> Domain1D
constant Boundary{n, i} = Domain1D{n, i, j = i}

isAtomic :: Domain1D -> Bool
isAtomic (Domain1D{n, i, j}) = (j .-. i) ./. n <= Number.epsilon

endpoints :: Domain1D -> (Boundary, Boundary)
endpoints (Domain1D{n, i, j}) = (Boundary{n, i}, Boundary{n, i = j})

lowerBoundary :: Domain1D -> Boundary
lowerBoundary (Domain1D{n, i}) = Boundary{n, i}

upperBoundary :: Domain1D -> Boundary
upperBoundary (Domain1D{n, j}) = Boundary{n, i = j}

midpoint :: Domain1D -> Boundary
midpoint (Domain1D n i j) = Boundary (2 *. n) (i .+. j)

width :: Domain1D -> Number
width (Domain1D n i j) = (j .-. i) ./. n

bisect :: Domain1D -> (Domain1D, Domain1D)
bisect (Domain1D{n, i, j}) = do
  let n2 = 2 *. n
  let i2 = 2 *. i
  let j2 = 2 *. j
  let mid = i2 .+. (j .-. i)
  (Domain1D n2 i2 mid, Domain1D n2 mid j2)

half :: Domain1D -> Domain1D
half (Domain1D{n, i, j}) = do
  let delta = j .-. i
  Domain1D (4 *. n) (4 *. i .+. delta) (4 *. j .-. delta)

bounds :: Domain1D -> Interval Unitless
bounds (Domain1D{n, i, j}) = Interval (i ./. n) (j ./. n)

interior :: Domain1D -> Interval Unitless
interior (Domain1D{n, i, j}) = do
  let n8 = 8 *. n
  let delta = j .-. i
  let low = if i == 0 then 0 else (8 *. i .+. delta) ./. n8
  let high = if j == n then 1 else (8 *. j .-. delta) ./. n8
  Interval low high

overlaps :: Domain1D -> Domain1D -> Bool
overlaps (Domain1D n2 i2 j2) (Domain1D n1 i1 j1) =
  i1 .*. n2 < j2 .*. n1 && j1 .*. n2 > i2 .*. n1

includes :: Boundary -> Domain1D -> Bool
includes (Boundary{n = pn, i = pi}) (Domain1D{n, i, j}) = do
  pi .*. n >= i .*. pn && pi .*. n <= j .*. pn

contains :: Domain1D -> Domain1D -> Bool
contains (Domain1D n2 i2 j2) (Domain1D n1 i1 j1) =
  i1 .*. n2 <= i2 .*. n1 && j1 .*. n2 >= j2 .*. n1

adjacent :: Domain1D -> Domain1D -> Bool
adjacent (Domain1D n1 i1 j1) (Domain1D n2 i2 j2) =
  i1 .*. n2 == j2 .*. n1 || j1 .*. n2 == i2 .*. n1

intersectionWidth :: Domain1D -> Domain1D -> Number
intersectionWidth (Domain1D n1 i1 j1) (Domain1D n2 i2 j2) = do
  let low = max (i1 ./. n1) (i2 ./. n2)
  let high = min (j1 ./. n1) (j2 ./. n2)
  max (high .-. low) 0

samplingPoints :: (Interval Unitless -> Bool) -> NonEmpty Number
samplingPoints predicate = 0 :| collectSamplingPoints predicate Interval.unit [1]

innerSamplingPoints :: (Interval Unitless -> Bool) -> List Number
innerSamplingPoints predicate = collectSamplingPoints predicate Interval.unit []

leadingSamplingPoints :: (Interval Unitless -> Bool) -> NonEmpty Number
leadingSamplingPoints predicate = 0 :| innerSamplingPoints predicate

trailingSamplingPoints :: (Interval Unitless -> Bool) -> NonEmpty Number
trailingSamplingPoints predicate =
  case collectSamplingPoints predicate Interval.unit [1] of
    NonEmpty points -> points
    [] -> do
      let message = "collectSamplingPoints should always return at least the point it was given"
      throw (InternalError message)

collectSamplingPoints :: (Interval Unitless -> Bool) -> Interval Unitless -> List Number -> List Number
collectSamplingPoints predicate subdomain accumulated
  | predicate subdomain = accumulated
  | Interval.isAtomic subdomain =
      throw (InternalError "Infinite recursion in Domain1D.samplingPoints")
  | otherwise = do
      let (left, right) = Interval.bisect subdomain
      let subdomainMidpoint = Interval.midpoint subdomain
      let rightAccumulated = collectSamplingPoints predicate right accumulated
      collectSamplingPoints predicate left (subdomainMidpoint : rightAccumulated)
