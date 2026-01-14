{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Bisection
  ( InfiniteRecursion (InfiniteRecursion)
  , Domain
  , Size (..)
  , Classification (..)
  , parameterDomain
  , curveDomain
  , curvePairDomain
  , surfaceDomain
  , curveSurfaceDomain
  , surfacePairDomain
  , search
  , interior
  , isInterior
  , includesEndpoint
  , classify
  , IsBounds (overlaps)
  )
where

import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Desingularization qualified as Desingularization
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.Number qualified as Number
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Result qualified as Result
import OpenSolid.UvBounds (UvBounds)

data InfiniteRecursion = InfiniteRecursion deriving (Eq, Show)

class IsBounds bounds where
  contains :: bounds -> bounds -> Bool
  overlaps :: bounds -> bounds -> Bool

instance IsBounds (Interval units) where
  contains = Interval.contains
  overlaps interval1 interval2 = Interval.overlap interval1 interval2 > Quantity.zero

instance IsBounds (Bounds2D units space) where
  contains = Bounds2D.contains
  overlaps (Bounds2D x1 y1) (Bounds2D x2 y2) = overlaps x1 x2 && overlaps y1 y2

instance (IsBounds bounds1, IsBounds bounds2) => IsBounds (bounds1, bounds2) where
  contains (b1, b2) (a1, a2) = contains b1 a1 && contains b2 a2
  overlaps (b1, b2) (a1, a2) = overlaps b1 a1 && overlaps b2 a2

data Domain bounds where
  Domain :: IsBounds bounds => bounds -> ~(List (Domain bounds)) -> Domain bounds

split :: Interval Unitless -> Domain (Interval Unitless)
split interval = Domain interval do
  let Interval low high = interval
  let mid = Number.midpoint low high
  let lowMid = Number.midpoint low mid
  let highMid = Number.midpoint mid high
  if mid > low && mid < high
    then [split (Interval low mid), shrink (Interval lowMid highMid), split (Interval mid high)]
    else []

shrink :: Interval Unitless -> Domain (Interval Unitless)
shrink interval = Domain interval do
  let Interval low high = interval
  let mid = Number.midpoint low high
  let lowMid = Number.midpoint low mid
  let highMid = Number.midpoint mid high
  [shrink (Interval lowMid highMid) | lowMid > low && highMid < high]

parameterDomain :: Domain (Interval Unitless)
parameterDomain = split Interval.unit

curveDomain :: Domain (Interval Unitless)
curveDomain = parameterDomain

curvePairDomain :: Domain (Interval Unitless, Interval Unitless)
curvePairDomain = map2 (,) curveDomain curveDomain

surfaceDomain :: Domain UvBounds
surfaceDomain = map2 Bounds2D parameterDomain parameterDomain

curveSurfaceDomain :: Domain (Interval Unitless, UvBounds)
curveSurfaceDomain = map2 (,) curveDomain surfaceDomain

surfacePairDomain :: Domain (UvBounds, UvBounds)
surfacePairDomain = map2 (,) surfaceDomain surfaceDomain

search ::
  Domain bounds ->
  (bounds -> Fuzzy (Maybe solution)) ->
  Result InfiniteRecursion (List (bounds, solution))
search domain callback = searchImpl [domain] callback []

searchImpl ::
  List (Domain bounds) ->
  (bounds -> Fuzzy (Maybe solution)) ->
  List (bounds, solution) ->
  Result InfiniteRecursion (List (bounds, solution))
searchImpl [] _ accumulated = Ok accumulated
searchImpl domains callback accumulated = do
  solutionsAndChildren <- Result.collect (visit callback accumulated) domains
  let (solutions, children) = List.unzip2 solutionsAndChildren
  searchImpl (List.concat children) callback (List.concat (accumulated : solutions))

visit ::
  (bounds -> Fuzzy (Maybe solution)) ->
  List (bounds, solution) ->
  Domain bounds ->
  Result InfiniteRecursion (List (bounds, solution), List (Domain bounds))
visit callback accumulated (Domain bounds children)
  | List.anySatisfy (contains bounds . Pair.first) accumulated = Ok ([], [])
  | otherwise = case callback bounds of
      Resolved Nothing -> Ok ([], [])
      Resolved (Just solution) -> Ok ([(bounds, solution)], [])
      Unresolved -> case children of
        [] -> Error InfiniteRecursion
        _ -> Ok ([], children)

map2 ::
  IsBounds bounds3 =>
  (bounds1 -> bounds2 -> bounds3) ->
  Domain bounds1 ->
  Domain bounds2 ->
  Domain bounds3
map2 function domain1 domain2 = do
  let Domain bounds1 children1 = domain1
  let Domain bounds2 children2 = domain2
  Domain (function bounds1 bounds2) $
    [ map2 function child1 child2
    | child1 <- children1
    , child2 <- children2
    ]

interior :: Interval Unitless -> Interval Unitless
interior domain = do
  let (# interiorLow, interiorHigh #) = interior# domain
  Interval interiorLow interiorHigh

isInterior :: Number -> Interval Unitless -> Bool
isInterior value domain = do
  let (# interiorLow, interiorHigh #) = interior# domain
  interiorLow <= value && value <= interiorHigh

{-# INLINE interior# #-}
interior# :: Interval Unitless -> (# Number, Number #)
interior# (Interval exteriorLow exteriorHigh) = do
  let margin = 0.125 *. (exteriorHigh .-. exteriorLow)
  let interiorLow = if exteriorLow == 0 then 0 else exteriorLow .+. margin
  let interiorHigh = if exteriorHigh == 1 then 1 else exteriorHigh .-. margin
  (# interiorLow, interiorHigh #)

includesEndpoint :: Interval Unitless -> Bool
includesEndpoint (Interval tLow tHigh) = tLow == 0 || tHigh == 1

data Size = Small | Large deriving (Eq, Ord, Show)

data Classification = Entire | Interior | Start Size | End Size

classify :: Interval Unitless -> Classification
classify (Interval low high)
  | 0 < low && high < 1 = Interior
  | high < 1 = Start (if high <= Desingularization.t0 then Small else Large)
  | 0 < low = End (if low >= Desingularization.t1 then Small else Large)
  | otherwise = Entire
