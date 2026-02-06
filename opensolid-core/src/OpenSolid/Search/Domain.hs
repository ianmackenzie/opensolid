{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Search.Domain
  ( Domain (Domain)
  , InfiniteRecursion (InfiniteRecursion)
  , Bounds
  , contains
  , touching
  , overlapping
  , isSmall
  , isBoundary
  , interior
  , unitInterval
  , pairwise
  , Size (..)
  , Classification (..)
  , classify
  )
where

import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Desingularization qualified as Desingularization
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Number qualified as Number
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.UvBounds (UvBounds, pattern UvBounds)

data Domain bounds where
  Domain :: Bounds bounds => bounds -> ~(List (Domain bounds)) -> Domain bounds

data InfiniteRecursion = InfiniteRecursion deriving (Show, Exception)

class Bounds bounds where
  contains :: bounds -> bounds -> Bool
  touching :: bounds -> bounds -> Bool
  overlapping :: bounds -> bounds -> Bool
  isSmall :: bounds -> Bool
  isBoundary :: bounds -> Bool
  interior :: bounds -> bounds

instance Bounds (Interval Unitless) where
  contains = Interval.contains
  touching interval1 interval2 = Interval.overlap interval1 interval2 >= 0
  overlapping interval1 interval2 = Interval.overlap interval1 interval2 > 0
  isSmall interval = Interval.width interval <= Desingularization.t0
  isBoundary = Parameter.hasEndpoint
  interior (Interval exteriorLow exteriorHigh) = do
    let margin = 0.125 *. (exteriorHigh .-. exteriorLow)
    let interiorLow = if exteriorLow == 0 then 0 else exteriorLow .+. margin
    let interiorHigh = if exteriorHigh == 1 then 1 else exteriorHigh .-. margin
    Interval interiorLow interiorHigh

instance Bounds UvBounds where
  contains = Bounds2D.contains
  touching (UvBounds u1 v1) (UvBounds u2 v2) = touching u1 u2 && touching v1 v2
  overlapping (UvBounds u1 v1) (UvBounds u2 v2) = overlapping u1 u2 && overlapping v1 v2
  isSmall (UvBounds u v) = isSmall u && isSmall v
  isBoundary (UvBounds u v) = isBoundary u || isBoundary v
  interior (UvBounds uBounds vBounds) = UvBounds (interior uBounds) (interior vBounds)

instance (Bounds bounds1, Bounds bounds2) => Bounds (bounds1, bounds2) where
  contains (b1, b2) (a1, a2) = contains b1 a1 && contains b2 a2
  touching (b1, b2) (a1, a2) = touching b1 a1 && touching b2 a2
  overlapping (b1, b2) (a1, a2) = overlapping b1 a1 && overlapping b2 a2
  isSmall (b1, b2) = isSmall b1 && isSmall b2
  isBoundary (b1, b2) = isBoundary b1 || isBoundary b2
  interior (b1, b2) = (interior b1, interior b2)

unitInterval :: Domain (Interval Unitless)
unitInterval = split Interval.unit

split :: Interval Unitless -> Domain (Interval Unitless)
split interval = do
  let (# low, lowMid, mid, highMid, high #) = quadrisect# interval
  Domain interval $
    [ split (Interval low mid)
    , shrink (Interval lowMid highMid)
    , split (Interval mid high)
    ]

shrink :: Interval Unitless -> Domain (Interval Unitless)
shrink interval = do
  let (# _, lowMid, _, highMid, _ #) = quadrisect# interval
  Domain interval [shrink (Interval lowMid highMid)]

quadrisect# :: Interval Unitless -> (# Number, Number, Number, Number, Number #)
quadrisect# (Interval low high) = do
  let mid = Number.midpoint low high
  let lowMid = Number.midpoint low mid
  let highMid = Number.midpoint mid high
  if low < lowMid && lowMid < mid && mid < highMid && highMid < high
    then (# low, lowMid, mid, highMid, high #)
    else throw InfiniteRecursion

pairwise :: Bounds c => (a -> b -> c) -> Domain a -> Domain b -> Domain c
pairwise function (Domain value1 children1) (Domain value2 children2) =
  Domain (function value1 value2) $
    [pairwise function child1 child2 | child1 <- children1, child2 <- children2]

data Size = Small | Large deriving (Eq, Ord, Show)

data Classification = Entire | Interior | Start Size | End Size

classify :: Interval Unitless -> Classification
classify (Interval 0 1) = Entire
classify (Interval 0 t) = Start (if t <= Desingularization.t0 then Small else Large)
classify (Interval t 1) = End (if t >= Desingularization.t1 then Small else Large)
classify (Interval _ _) = Interior
