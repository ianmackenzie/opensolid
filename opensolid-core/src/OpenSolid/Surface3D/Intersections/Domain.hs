module OpenSolid.Surface3D.Intersections.Domain
  ( Domain (Domain)
  , unitCube
  , contains
  , aggregate
  , Octants
  , octants
  )
where

import OpenSolid.Bisection qualified as Bisection
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Set qualified as Set
import OpenSolid.Set.Bounds qualified as Set.Bounds
import OpenSolid.Struct qualified as Struct

data Domain = Domain (Interval Unitless) (Interval Unitless) (Interval Unitless)

instance Intersects Domain Domain Unitless where
  {-# INLINE (^) #-}
  Domain r1 f1 u1 ^ Domain r2 f2 u2 = r1 ^ r2 && f1 ^ f2 && u1 ^ u2

instance Set.Bounds Domain where
  dimension _ = 3
  sortValue index (Domain r f u) = case index of
    0 -> Interval.midpoint r
    1 -> Interval.midpoint f
    2 -> Interval.midpoint u
    _ -> error "Index out of bounds"
  aggregateOf function bounds = aggregate (NonEmpty.map function bounds)

instance Bisection.Domain Domain where
  {-# INLINE contains #-}
  contains = contains

unitCube :: Domain
unitCube = Domain Interval.unit Interval.unit Interval.unit

contains :: Domain -> Domain -> Bool
contains (Domain r2 f2 u2) (Domain r1 f1 u1) =
  Interval.contains r2 r1 && Interval.contains f2 f1 && Interval.contains u2 u1

aggregate :: NonEmpty Domain -> Domain
aggregate (first :| rest) = do
  let Domain (Interval rLow rHigh) (Interval fLow fHigh) (Interval uLow uHigh) = first
  aggregateImpl rLow rHigh fLow fHigh uLow uHigh rest

aggregateImpl ::
  Number ->
  Number ->
  Number ->
  Number ->
  Number ->
  Number ->
  List Domain ->
  Domain
aggregateImpl rLow rHigh fLow fHigh uLow uHigh rest = case rest of
  [] -> Domain (Interval rLow rHigh) (Interval fLow fHigh) (Interval uLow uHigh)
  next : remaining -> do
    let Domain rNext fNext uNext = next
    let Interval rLowNext rHighNext = rNext
    let Interval fLowNext fHighNext = fNext
    let Interval uLowNext uHighNext = uNext
    aggregateImpl
      (min rLow rLowNext)
      (max rHigh rHighNext)
      (min fLow fLowNext)
      (max fHigh fHighNext)
      (min uLow uLowNext)
      (max uHigh uHighNext)
      remaining

type Octants =
  ( "bottomBackLeft" ::: Domain
  , "bottomBackRight" ::: Domain
  , "bottomFrontLeft" ::: Domain
  , "bottomFrontRight" ::: Domain
  , "topBackLeft" ::: Domain
  , "topBackRight" ::: Domain
  , "topFrontLeft" ::: Domain
  , "topFrontRight" ::: Domain
  )

octants :: Domain -> Octants
octants (Domain rightward forward upward) = do
  let (left, right) = Interval.bisect rightward
  let (back, front) = Interval.bisect forward
  let (bottom, top) = Interval.bisect upward
  Struct.with
    { bottomBackLeft = Domain left back bottom
    , bottomBackRight = Domain right back bottom
    , bottomFrontLeft = Domain left front bottom
    , bottomFrontRight = Domain right front bottom
    , topBackLeft = Domain left back top
    , topBackRight = Domain right back top
    , topFrontLeft = Domain left front top
    , topFrontRight = Domain right front top
    }
