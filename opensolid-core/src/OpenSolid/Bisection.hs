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
  )
where

import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Desingularization qualified as Desingularization
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.List qualified as List
import OpenSolid.Number qualified as Number
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Result qualified as Result
import OpenSolid.UvBounds (UvBounds)

data InfiniteRecursion = InfiniteRecursion deriving (Eq, Show)

class IsBounds bounds where
  contains :: bounds -> bounds -> Bool

instance IsBounds (Bounds units) where
  contains = Bounds.contains

instance IsBounds (Bounds2d units space) where
  contains = Bounds2d.contains

instance (IsBounds bounds1, IsBounds bounds2) => IsBounds (bounds1, bounds2) where
  contains (b1, b2) (a1, a2) = contains b1 a1 && contains b2 a2

data Domain bounds where
  Domain :: IsBounds bounds => bounds -> ~(List (Domain bounds)) -> Domain bounds

split :: Bounds Unitless -> Domain (Bounds Unitless)
split bounds = Domain bounds do
  let Bounds low high = bounds
  let mid = Number.midpoint low high
  let lowMid = Number.midpoint low mid
  let highMid = Number.midpoint mid high
  if mid > low && mid < high
    then [split (Bounds low mid), shrink (Bounds lowMid highMid), split (Bounds mid high)]
    else []

shrink :: Bounds Unitless -> Domain (Bounds Unitless)
shrink bounds = Domain bounds do
  let Bounds low high = bounds
  let mid = Number.midpoint low high
  let lowMid = Number.midpoint low mid
  let highMid = Number.midpoint mid high
  [shrink (Bounds lowMid highMid) | lowMid > low && highMid < high]

parameterDomain :: Domain (Bounds Unitless)
parameterDomain = split Bounds.unitInterval

curveDomain :: Domain (Bounds Unitless)
curveDomain = parameterDomain

curvePairDomain :: Domain (Bounds Unitless, Bounds Unitless)
curvePairDomain = map2 (,) curveDomain curveDomain

surfaceDomain :: Domain UvBounds
surfaceDomain = map2 Bounds2d parameterDomain parameterDomain

curveSurfaceDomain :: Domain (Bounds Unitless, UvBounds)
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

interior :: Bounds Unitless -> Bounds Unitless
interior (Bounds tLow tHigh) = do
  let margin = (tHigh .-. tLow) ./ 8
  let lowMargin = if tLow == 0 then 0 else margin
  let highMargin = if tHigh == 1 then 0 else margin
  Bounds (tLow + lowMargin) (tHigh - highMargin)

isInterior :: Number -> Bounds Unitless -> Bool
isInterior value (Bounds tLow tHigh) = do
  let margin = (tHigh .-. tLow) ./ 8
  let lowMargin = if tLow == 0 then 0 else margin
  let highMargin = if tHigh == 1 then 0 else margin
  value >= tLow .+. lowMargin && value <= tHigh .-. highMargin

includesEndpoint :: Bounds Unitless -> Bool
includesEndpoint (Bounds tLow tHigh) = tLow == 0 || tHigh == 1

data Size = Small | Large deriving (Eq, Ord, Show)

data Classification = Entire | Interior | Start Size | End Size

classify :: Bounds Unitless -> Classification
classify (Bounds low high)
  | 0 < low && high < 1 = Interior
  | high < 1 = Start (if high <= Desingularization.t0 then Small else Large)
  | 0 < low = End (if low >= Desingularization.t1 then Small else Large)
  | otherwise = Entire
