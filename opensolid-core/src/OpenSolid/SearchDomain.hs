module OpenSolid.SearchDomain
  ( SearchDomain
  , InfiniteRecursion (InfiniteRecursion)
  , Bounds
  , touching
  , bounds
  , children
  , contains
  , overlapping
  , isSmall
  , isPrimary
  , curve
  , surface
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
import OpenSolid.Prelude
import OpenSolid.UvBounds (UvBounds, pattern UvBounds)

data SearchDomain bounds = SearchDomain
  { bounds :: bounds
  , children :: ~(List (SearchDomain bounds))
  }

data InfiniteRecursion = InfiniteRecursion deriving (Show, Exception)

class Bounds bounds where
  contains :: bounds -> bounds -> Bool
  overlap :: bounds -> bounds -> Number
  isSmall :: bounds -> Bool
  isPrimary :: bounds -> Bool

instance Bounds (Interval Unitless) where
  contains = Interval.contains
  overlap = Interval.overlap
  isSmall interval = Interval.width interval <= Desingularization.t0
  isPrimary (Interval low high) = do
    let n = low / (high - low)
    Number.floor n == Number.ceiling n

instance Bounds UvBounds where
  contains = Bounds2D.contains
  overlap = Bounds2D.overlap
  isSmall (UvBounds u v) = isSmall u && isSmall v
  isPrimary (UvBounds u v) = isPrimary u && isPrimary v

instance (Bounds bounds1, Bounds bounds2) => Bounds (bounds1, bounds2) where
  contains (b1, b2) (a1, a2) = contains b1 a1 && contains b2 a2
  overlap (b1, b2) (a1, a2) = min (overlap b1 a1) (overlap b2 a2)
  isSmall (b1, b2) = isSmall b1 && isSmall b2
  isPrimary (b1, b2) = isPrimary b1 && isPrimary b2

touching :: Bounds b => b -> b -> Bool
touching bounds1 bounds2 = overlap bounds1 bounds2 >= 0.0

overlapping :: Bounds b => b -> b -> Bool
overlapping bounds1 bounds2 = overlap bounds1 bounds2 > 0.0

bounds :: forall bounds. SearchDomain bounds -> bounds
bounds = (.bounds)

children :: forall bounds. SearchDomain bounds -> List (SearchDomain bounds)
children = (.children)

unitInterval :: SearchDomain (Interval Unitless)
unitInterval = split Interval.unit

curve :: SearchDomain (Interval Unitless)
curve = unitInterval

surface :: SearchDomain UvBounds
surface = pairwise UvBounds unitInterval unitInterval

split :: Interval Unitless -> SearchDomain (Interval Unitless)
split interval = do
  let (low, lowMid, mid, highMid, high) = quadrisect interval
  SearchDomain interval $
    [ split (Interval low mid)
    , shrink (Interval lowMid highMid)
    , split (Interval mid high)
    ]

shrink :: Interval Unitless -> SearchDomain (Interval Unitless)
shrink interval = do
  let (_, lowMid, _, highMid, _) = quadrisect interval
  SearchDomain interval [shrink (Interval lowMid highMid)]

{-# INLINE quadrisect #-}
quadrisect :: Interval Unitless -> (Number, Number, Number, Number, Number)
quadrisect (Interval low high) = do
  let mid = Number.midpoint low high
  let lowMid = Number.midpoint low mid
  let highMid = Number.midpoint mid high
  if low < lowMid && lowMid < mid && mid < highMid && highMid < high
    then (low, lowMid, mid, highMid, high)
    else throw InfiniteRecursion

pairwise :: (a -> b -> c) -> SearchDomain a -> SearchDomain b -> SearchDomain c
pairwise function (SearchDomain bounds1 children1) (SearchDomain bounds2 children2) =
  SearchDomain (function bounds1 bounds2) $
    [pairwise function child1 child2 | child1 <- children1, child2 <- children2]

data Size = Small | Large deriving (Eq, Ord, Show)

data Classification = Entire | Interior | Start Size | End Size

classify :: Interval Unitless -> Classification
classify (Interval 0.0 1.0) = Entire
classify (Interval 0.0 t) = Start (if t <= Desingularization.t0 then Small else Large)
classify (Interval t 1.0) = End (if t >= Desingularization.t1 then Small else Large)
classify (Interval _ _) = Interior
